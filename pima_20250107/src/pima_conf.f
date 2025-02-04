#include <mk5_preprocessor_directives.inc>
      SUBROUTINE PIMA_CONF ( CONF_FILE, PIM, L_OPT, KEYWORD, VALUE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_CONF parses control file for pima and puts results of *
! *   parsing inside PIM data structure.                                 *
! *                                                                      *
! *  ### 06-JAN-2006   PIMA_CONF  v1.45 (c)  L. Petrov  08-JAN-2023  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'fftw3.f'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      CHARACTER  CONF_FILE*(*), &
     &           KEYWORD(PIM__MOPT)*(*), VALUE(PIM__MOPT)*(*)
      INTEGER*4  L_OPT, IUER
      INTEGER*4  MIND, MBUF
      PARAMETER  ( MIND =   32 )
      PARAMETER  ( MBUF = 1024 )
      CHARACTER  STR*128, STR1*128, STR2*4096, FIL_STT*128, PCAL_USE_STR*8, &
     &           BUF(MBUF)*128, BUFS(MBUF)*128, REG*3
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      CHARACTER, ALLOCATABLE :: BUF_OBS(:)*128, BUF_SCA(:)*128
      CHARACTER  PIMA__LABEL1*50, SCA_COMMAND*128, FIL_NAME*128
      INTEGER*4  IS, UNIX_DATE, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, &
     &           J11, J12, J13, LIND, IND(2,MIND), NBUF, LKEY, LFRIB_KEY, &
     &           LFRIP_KEY, LMKDB_KEY, LBPS_KEY, LSPLT_KEY, LONOF_KEY, &
     &           IP, IH, IL, IVAL, IVALS, IVAL1, IVAL2, NB, NS, ID, &
     &           LIND_SCA, IND_SCA(2,MIND), NPCL, IER
      INTEGER*2  MASK_DIR
      DATA       MASK_DIR / O'775' /
      INTEGER*8  DIR_DESC
      LOGICAL*4  LEX, FL_FOUND, FL_KEY_OVER
      INTEGER*4  SIZE_I8
      INTEGER*4, EXTERNAL :: FILE_INFO, ILEN, I_LEN, LINDEX, LTM_DIF, MKDIR, SETENV
      INTEGER*8, EXTERNAL :: FUNC_OPENDIR
!
! --- Get information about the control file
!
      IS = FILE_INFO ( CONF_FILE(1:I_LEN(CONF_FILE))//CHAR(0), UNIX_DATE, &
     &                 SIZE_I8 )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 8101, IUER, 'PIMA_CONF', 'Error in accessing '// & 
    &         ' control file '//CONF_FILE(1:I_LEN(CONF_FILE))//' -- '//STR )
           RETURN
      END IF
      IF ( SIZE_I8 .EQ. 0 ) THEN
           CALL ERR_LOG ( 8102, IUER, 'PIMA_CONF', 'Control file '// &
     &          CONF_FILE(1:I_LEN(CONF_FILE))//' haz zero length' )
           RETURN
      END IF
!
! --- Read the control file into the buffer BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( CONF_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8103, IUER, 'PIMA_CONF', 'Error in an attempt to '// &
     &         'read control file '//CONF_FILE )
           RETURN
      END IF
!
! --- Check the first and the last line of the control file.
! --- They should have format labels
!
      IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) .NE. PIMA__CONTROL_LABEL ) THEN
           IF ( BUF(1)(1:39) == '# PIMA_CONTROL file.  Format Version of' ) THEN
                CALL ERR_LOG ( 8104, IUER, 'PIMA_CONF', 'Control file '// &
     &               CONF_FILE(1:I_LEN(CONF_FILE))//' has too old version '// &
     &              'and should be upgraded with task upgr as '// &
     &              'pima '//TRIM(CONF_FILE)//' upgr' )
                RETURN
             ELSE 
                CALL ERR_LOG ( 8105, IUER, 'PIMA_CONF', 'The first line of '// &
     &              'control file '//CONF_FILE(1:I_LEN(CONF_FILE))//' does not '// &
     &              'contain a control file label '//PIMA__CONTROL_LABEL// &
     &              ' -- Is that really a control file?' )
                RETURN
           END IF
      END IF
!
      IF ( BUF(NBUF) .NE. BUF(1) ) THEN
           CALL ERR_LOG ( 8106, IUER, 'PIMA_CONF', 'The last line of '// &
     &         'control file '//CONF_FILE(1:I_LEN(CONF_FILE))//' does not '// &
     &         'contain a format label. Was the file read to the end?' )
           RETURN
      END IF
!
      FL_KEY_OVER = .FALSE.
      IF ( L_OPT > 0 ) THEN
!
! -------- Replace value of keywords in the buffer with values from the
! -------- command-line options
!
           DO 410 J1=1,L_OPT
              IF ( PIM%CONF%ACT_CODE == PIMA__GEAN_CODE ) THEN
!
! ---------------- Bypass an option for operation gean
!
                   IF ( KEYWORD(J1) == 'evn_antab_file'  ) GOTO 410
                   IF ( KEYWORD(J1) == 'evn_gain'        ) GOTO 410
                   IF ( KEYWORD(J1) == 'pima_antab_file' ) GOTO 410
                   IF ( KEYWORD(J1) == 'vlba_log_file'   ) GOTO 410
                   IF ( KEYWORD(J1) == 'vlba_gain'       ) GOTO 410
                   IF ( KEYWORD(J1) == 'num_tones'       ) GOTO 410
                   IF ( KEYWORD(J1) == 'pcal_dir'        ) GOTO 410
                   IF ( KEYWORD(J1) == 'gain_band'       ) GOTO 410
                   IF ( KEYWORD(J1) == 'lvim_dir'        ) GOTO 410
                   IF ( KEYWORD(J1) == 'pcal_on'         ) GOTO 410
                   IF ( KEYWORD(J1) == 'pcal_off'        ) GOTO 410
                   IF ( KEYWORD(J1) == 'tsys_on'         ) GOTO 410
                   IF ( KEYWORD(J1) == 'tsys_off'        ) GOTO 410
                   IF ( KEYWORD(J1) == 'wvr'             ) GOTO 410
              END IF
!
              IF ( PIM%CONF%ACT_CODE == PIMA__BMGE_CODE .OR. &
     &             PIM%CONF%ACT_CODE == PIMA__PMGE_CODE ) THEN
!
! ---------------- Bypass an option for operation bmge or pmge
!
                   IF ( KEYWORD(J1) == 'mask_gen'        ) GOTO 410
              END IF
!
              IF ( PIM%CONF%ACT_CODE == PIMA__BPLT_CODE .OR. &
     &             PIM%CONF%ACT_CODE == PIMA__PPLT_CODE      ) THEN
!
! ---------------- Bypass an option for operation bplt
!
                   IF ( KEYWORD(J1) == 'sta1'        ) GOTO 410
                   IF ( KEYWORD(J1) == 'sta2'        ) GOTO 410
                   IF ( KEYWORD(J1) == 'plot_file'   ) GOTO 410
              END IF 
              IF ( PIM%CONF%ACT_CODE == PIMA__OPAG_CODE ) THEN
!
! ---------------- Bypass an option for operation opag
!
                  IF ( KEYWORD(J1) == 'spd_url'    ) GOTO 410
              END IF
              IF ( PIM%CONF%ACT_CODE == PIMA__GENA_CODE ) THEN
!
! ---------------- Bypass an option for operation opag
!
                  IF ( KEYWORD(J1) == 'sta'  ) GOTO 410
                  IF ( KEYWORD(J1) == 'type' ) GOTO 410
                  IF ( KEYWORD(J1) == 'file' ) GOTO 410
                  IF ( KEYWORD(J1) == 'trec' ) GOTO 410
                  IF ( KEYWORD(J1) == 'tatm' ) GOTO 410
              END IF
              IF ( PIM%CONF%ACT_CODE == PIMA__PCPL_CODE ) THEN
                   IF ( KEYWORD(J1)(1:9) == 'pcal_type'      ) GOTO 410
              END IF
              IF ( PIM%CONF%ACT_CODE == PIMA__TSPL_CODE ) THEN
!
! ---------------- Bypass an option for operation tspl
!
                   IF ( KEYWORD(J1) == 'data'    ) GOTO 410
              END IF
              IF ( PIM%CONF%ACT_CODE == PIMA__GACO_CODE ) THEN
                   IF ( KEYWORD(J1)(1:3) == 'ini' ) GOTO 410
                   IF ( KEYWORD(J1)(1:3) == 'dir' ) GOTO 410
                   IF ( KEYWORD(J1)(1:3) == 'sou' ) GOTO 410
              END IF
              IF ( PIM%CONF%ACT_CODE == PIMA__TSMO_CODE ) THEN
                   IF ( KEYWORD(J1)(1:4) == 'mode'    ) GOTO 410
              END IF
              IF ( PIM%CONF%ACT_CODE == PIMA__GEPM_CODE ) THEN
                   IF ( KEYWORD(J1)(1:3) == 'sta'      ) GOTO 410
                   IF ( KEYWORD(J1)(1:8) == 'tim_mseg' ) GOTO 410
                   IF ( KEYWORD(J1)(1:4) == 'over'     ) THEN
                        FL_KEY_OVER = .TRUE.
                        GOTO 410
                   END IF 
                   IF ( KEYWORD(J1)(1:10) == 'tim_thresh' ) GOTO 410
                   IF ( KEYWORD(J1)(1:11) == 'diff_thresh' ) GOTO 410
                   IF ( KEYWORD(J1)(1:9) == 'max_count' ) GOTO 410
              END IF
!
              IF ( KEYWORD(J1) == 'FRIB.OBS:' ) KEYWORD(J1) = 'OBS:' 
              IL = ILEN(KEYWORD(J1))
              IF ( KEYWORD(J1)(IL:IL) .NE. ':' ) THEN
                   IL = IL + 1
                   KEYWORD(J1)(IL:IL) = ':'
              END IF
!
              FL_FOUND = .FALSE.
              DO 420 J2=2,NBUF-1
                 IF ( BUF(J2)(1:1)  == '#' ) GOTO 420
                 IF ( ILEN(BUF(J2)) ==  0  ) GOTO 420
                 IF ( BUF(J2)(1:9) == 'FRIB.OBS:' ) BUF(J2)(1:9) = 'OBS:     '
                 IF ( BUF(J2)(1:IL) == KEYWORD(J1)(1:IL) ) THEN
                      FL_FOUND = .TRUE.
                      CALL EXWORD ( BUF(J2), MIND, LIND, IND, REG, -3 )
                      IF ( LIND < 2 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( J2, STR )
                           CALL ERR_LOG ( 8107, IUER, 'PIMA_CONF', 'Error '// &
     &                         'in parsing the '//STR(1:I_LEN(STR))// &
     &                         '-th line of the control file '// &
     &                          CONF_FILE(1:I_LEN(CONF_FILE))//' -- "'// &
     &                          BUF(J2)(1:I_LEN(BUF(J2)))//'" -- too few words' )
                           RETURN
                      END IF
                      CALL CLRCH ( BUF(J2)(IND(1,2):) )
                      BUF(J2)(IND(1,2):) = VALUE(J1)
                 END IF
 420          CONTINUE
              IF ( .NOT. FL_FOUND ) THEN
                   IF ( KEYWORD(J1)(1:8) == 'PIMAVAR_' ) THEN
                        NBUF = NBUF + 1
                        BUF(NBUF) =  BUF(NBUF-1)
                        CALL CLRCH ( BUF(NBUF-1) )
                        BUF(NBUF-1) = KEYWORD(J1)(1:IL)//'  '// &
     &                                VALUE(J1)
                     ELSE
                        CALL ERR_PASS ( IUER, IER )
                        CALL ERR_LOG ( 8108, IUER, 'PIMA_CONF', 'Keyword '// &
     &                       KEYWORD(J1)(1:IL)//' specified as the command '// &
     &                      'line option is not supported' )
                        IF ( PIM%CONF%ACT_CODE == PIMA__BPLT_CODE ) THEN
                             CALL ERR_LOG ( 8109, IER, 'PIMA_CONF', 'suppored '// &
     &                           'keywords: sta1, sta2, plot_file' )
                        END IF
                        RETURN
                   END IF
              END IF
 410       CONTINUE
      END IF
!
! --- Parse the buffer with control file
!
      LKEY = 0
      LFRIB_KEY = 0
      LFRIP_KEY = 0
      LBPS_KEY  = 0
      LMKDB_KEY = 0
      LSPLT_KEY = 0
      LONOF_KEY = 0
      DO 430 J3=2,NBUF-1
         CALL CLRCH (     STR )
         CALL INCH  ( J3, STR )
         IF ( BUF(J3)(1:1)  == '#' ) GOTO 430
         IF ( ILEN(BUF(J3)) ==  0  ) GOTO 430
!
         CALL ERR_PASS ( IUER, IER )
         CALL RESOLVE_ENV ( BUF(J3), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8110, IUER, 'PIMA_CONF', 'Failure in an attempt '// &
     &            'to resolve environment varaibles when processing '// &
     &            'the '//STR(1:I_LEN(STR))//'-th line of the control file '// &
     &             CONF_FILE(1:I_LEN(CONF_FILE))//' -- "'// &
     &             BUF(J3)(1:I_LEN(BUF(J3)))//'"' )
              RETURN
         END IF
!
! ------ Parse the line into keywords
!
         CALL EXWORD ( BUF(J3), MIND, LIND, IND, REG, -3 )
         IF ( LIND < 2 ) THEN
              CALL ERR_LOG ( 8111, IUER, 'PIMA_CONF', 'Error during parsing '// &
     &            'the '//STR(1:I_LEN(STR))//'-th line of the control file '// &
     &             CONF_FILE(1:I_LEN(CONF_FILE))//' -- "'// &
     &             BUF(J3)(1:I_LEN(BUF(J3)))//'" -- too few words' )
              RETURN
         END IF
!
         IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SESS_CODE:' ) THEN
              PIM%CONF%SESS_CODE = BUF(J3)(IND(1,2):IND(2,2))
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BAND:' ) THEN
              PIM%CONF%BAND =  BUF(J3)(IND(1,2):IND(2,2))
              CALL TRAN ( 11, PIM%CONF%BAND, PIM%CONF%BAND )
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'STAGING_DIR:' ) THEN
              PIM%CONF%STAGING_DIR =  BUF(J3)(IND(1,2):IND(2,2))
              IF ( PIM%CONF%STAGING_DIR(1:2) == 'NO' ) THEN
                   CALL CLRCH ( PIM%CONF%STAGING_DIR ) 
                 ELSE 
!
! ---------------- Check whethe directory exists
!
                   DIR_DESC = FUNC_OPENDIR ( PIM%CONF%STAGING_DIR(1:I_LEN(PIM%CONF%STAGING_DIR))//CHAR(0) )
                   IF ( DIR_DESC .EQ. 0 ) THEN
!
! --------------------- Does not exist. Then create it
!
                        IS = MKDIR ( PIM%CONF%STAGING_DIR(1:I_LEN(PIM%CONF%STAGING_DIR))//CHAR(0), &
     &                               %VAL(MASK_DIR) )
                        IF ( IS .NE. 0 ) THEN
                             CALL GERROR ( STR )
                             CALL ERR_LOG ( 8112, IUER, 'PIMA_CONF', 'Error during '// &
     &                           'attempt to read staging directory '// &
     &                            PIM%CONF%STAGING_DIR(1:I_LEN(PIM%CONF%STAGING_DIR))// &
     &                           ' -- '//STR )
                             RETURN
                       END IF
                     ELSE
                       CALL CLOSEDIR ( %VAL(DIR_DESC) )
                   END IF
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'FRINGE_ERRORS:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__ERR_IGNORE ) THEN
                   PIM%CONF%FRINGE_ERRORS = PIMA__ERR_IGNORE
                ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__ERR_CORRECT ) THEN
                   PIM%CONF%FRINGE_ERRORS = PIMA__ERR_CORRECT
                ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__ERR_STOP    ) THEN
                   PIM%CONF%FRINGE_ERRORS = PIMA__ERR_STOP
                ELSE
                   CALL ERR_LOG ( 8113, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong value of the keyword FRINGE_ERRORS: '// &
     &                 BUF(J3)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'DEBUG_LEVEL:' ) THEN
              CALL CHIN ( BUF(J3)(IND(1,2):IND(2,2)), PIM%CONF%DEBUG_LEVEL )
              IF ( PIM%CONF%DEBUG_LEVEL .LT. 0  .OR. &
                   PIM%CONF%DEBUG_LEVEL  > 999       ) THEN
                   CALL ERR_LOG ( 8114, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- either ill formated or out of range value of '// &
     &                 'the keyword DEBUG_LEVEL '//BUF(J3)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'CHECK_SEVERITY:' ) THEN
              CALL CHIN ( BUF(J3)(IND(1,2):IND(2,2)), PIM%CONF%CHECK_SEVERITY )
              IF ( PIM%CONF%CHECK_SEVERITY .LT. 0 .OR. &
                   PIM%CONF%CHECK_SEVERITY > 999       ) THEN
                   CALL ERR_LOG ( 8115, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- either ill formated or out of range value of '// &
     &                 'the keyword CHECK_SEVERITY '// &
     &                  BUF(J3)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'WARNING:' ) THEN
              CALL TRAN ( 11, BUF(J3)(IND(1,2):IND(2,2)), BUF(J3)(IND(1,2):IND(2,2)) )
              IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'YES' ) THEN
                   PIM%CONF%WARNING = .TRUE.
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'ON' ) THEN
                   PIM%CONF%WARNING = .TRUE.
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'NO' ) THEN
                   PIM%CONF%WARNING = .FALSE.
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'OFF' ) THEN
                   PIM%CONF%WARNING = .FALSE.
                 ELSE
                   CALL ERR_LOG ( 8116, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword WARNING: only YES or NO are supported' )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SOU_NAMES:' ) THEN
              PIM%CONF%SOU_NAMES_FILE = BUF(J3)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=PIM%CONF%SOU_NAMES_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 8117, IUER, 'PIMA_CONF', 'Cannot find '// &
     &                 'input source name file '// &
     &                  PIM%CONF%SOU_NAMES_FILE(1:I_LEN(PIM%CONF%SOU_NAMES_FILE))// &
     &                 ' specified in the control file '//CONF_FILE )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'STA_NAMES:' ) THEN
              PIM%CONF%STA_NAMES_FILE = BUF(J3)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=PIM%CONF%STA_NAMES_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 8118, IUER, 'PIMA_CONF', 'Cannot find '// &
     &                 'input station name file '// &
     &                  PIM%CONF%STA_NAMES_FILE(1:I_LEN(PIM%CONF%STA_NAMES_FILE))// &
     &                 ' specified in the control file '//CONF_FILE )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'AP_TOLERANCE:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F20.12)', &
     &               IOSTAT=IER  ) PIM%CONF%AP_TOLERANCE
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8119, IUER, 'PIMA_CONF', 'Error in '// &
     &                 'an attempt to decode the AP_TOLERANCE '// &
     &                 BUF(J3)(IND(1,1):IND(2,1))//' in the control file '// &
     &                 CONF_FILE )
                   RETURN
              END IF
              IF ( PIM%CONF%AP_TOLERANCE < 0.0D0 ) THEN
                   CALL ERR_LOG ( 8120, IUER, 'PIMA_CONF', 'Error in '// &
     &                 'an attempt to decode the AP_TOLERANCE '// &
     &                 BUF(J3)(IND(1,1):IND(2,1))//' in the control file '// &
     &                 CONF_FILE(1:I_LEN(CONF_FILE))//' it should be positive' )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'MIN_SCAN_LEN:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F20.12)', &
     &               IOSTAT=IER  ) PIM%CONF%MIN_SCAN_LEN
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8121, IUER, 'PIMA_CONF', 'Error in '// &
     &                 'an attempt to decode the MIN_SCAN_LEN '// &
     &                 BUF(J3)(IND(1,1):IND(2,1))//' in the control file '// &
     &                 CONF_FILE )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'MAX_SCAN_LEN:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F20.12)', &
     &               IOSTAT=IER  ) PIM%CONF%MAX_SCAN_LEN
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8122, IUER, 'PIMA_CONF', 'Error in '// &
     &                 'an attempt to decode the MAX_SCAN_LEN '// &
     &                 BUF(J3)(IND(1,1):IND(2,1))//' in the control file '// &
     &                 CONF_FILE )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'MAX_SCAN_GAP:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F20.12)', &
     &               IOSTAT=IER  ) PIM%CONF%MAX_SCAN_GAP
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8123, IUER, 'PIMA_CONF', 'Error in '// &
     &                 'an attempt to decode the MAX_SCAN_GAP '// &
     &                 BUF(J3)(IND(1,1):IND(2,1))//' in the control file '// &
     &                 CONF_FILE )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SCAN_LEN_SKIP:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F20.12)', &
     &               IOSTAT=IER  ) PIM%CONF%SCAN_LEN_SKIP
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8124, IUER, 'PIMA_CONF', 'Error in '// &
     &                 'an attempt to decode the SCAN_LEN_SKIP '// &
     &                 BUF(J3)(IND(1,1):IND(2,1))//' in the control file '// &
     &                 CONF_FILE )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SCAN_LEN_USED:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F20.12)', &
     &               IOSTAT=IER  ) PIM%CONF%SCAN_LEN_USED
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8125, IUER, 'PIMA_CONF', 'Error in '// &
     &                 'an attempt to decode the SCAN_LEN_USED '// &
     &                 BUF(J3)(IND(1,1):IND(2,1))//' in the control file '// &
     &                 CONF_FILE )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'FRT_OFFSET:' ) THEN
              CALL CLRCH ( PIM%CONF%FRT_FILE )
              PIM%CONF%FRT_OFFSET = 0.0D0
              PIM%CONF%FRT_USE = PIMA__FRT_AUTO
              IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'AUTO' ) THEN
                   PIM%CONF%FRT_USE = PIMA__FRT_AUTO
                ELSE
                   PIM%CONF%FRT_USE = PIMA__FRT_VAL
                   IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                        BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                        IND(2,2) = IND(2,2) + 2
                   END IF
                   READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F20.12)', &
     &                    IOSTAT=IER  ) PIM%CONF%FRT_OFFSET
                   IF ( IER .NE. 0 ) THEN
                        PIM%CONF%FRT_FILE = BUF(J3)(IND(1,2):IND(2,2))
                        INQUIRE ( FILE=PIM%CONF%FRT_FILE, EXIST=LEX )
                        IF ( .NOT. LEX ) THEN
                             CALL ERR_LOG ( 8126, IUER, 'PIMA_CONF', 'Error in '// &
     &                           'an attempt to decode the FRT_OFFSET '// &
     &                           BUF(J3)(IND(1,1):IND(2,1))// &
     &                           ' in the control file '// &
     &                           CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                           ' -- it should be either AUTO, or '// &
     &                           'REAL*8 constant, or valid file name' )
                             RETURN
                        END IF
                        PIM%CONF%FRT_USE = PIMA__FRT_FILE
                   END IF
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'STA_REF:' ) THEN
              CALL CLRCH ( PIM%CONF%STA_REF )
              PIM%CONF%STA_REF = BUF(J3)(IND(1,2):IND(2,2))
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'UV_FITS:' ) THEN
              PIM%CONF%L_FIL = PIM%CONF%L_FIL + 1
              IF ( PIM%CONF%L_FIL > PIM__MFIL ) THEN
                   CALL CLRCH (            STR )
                   CALL INCH  ( PIM__MFIL, STR )
                   CALL ERR_LOG ( 8127, IUER, 'PIMA_CONF', 'Too many UV '// &
     &                 'input files specified in the control file '// &
     &                  CONF_FILE(1:I_LEN(CONF_FILE))//' more than the '// &
     &                 'limit PIM__MFIL '//STR )
                   RETURN
              END IF
!
              PIM%CONF%UVFILE_NAME(PIM%CONF%L_FIL) = BUF(J3)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=PIM%CONF%UVFILE_NAME(PIM%CONF%L_FIL), EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   IF ( ILEN(PIM%CONF%STAGING_DIR) > 0 ) THEN
                        ID = LINDEX ( PIM%CONF%UVFILE_NAME(PIM%CONF%L_FIL), '/' )
                        FIL_NAME = PIM%CONF%STAGING_DIR(1:I_LEN(PIM%CONF%STAGING_DIR))// &
     &                             PIM%CONF%UVFILE_NAME(PIM%CONF%L_FIL)(ID:)
                        INQUIRE ( FILE=FIL_NAME, EXIST=LEX )
                   END IF
                   IF ( .NOT. LEX ) THEN
                        IP = I_LEN(PIM%CONF%UVFILE_NAME(PIM%CONF%L_FIL))
                        CALL ERR_LOG ( 8128, IUER, 'PIMA_CONF', 'Cannot find '// &
     &                      'input UV file '// &
     &                       PIM%CONF%UVFILE_NAME(PIM%CONF%L_FIL)(1:IP)// &
     &                      ' specified in the control file '//CONF_FILE )
                        RETURN
                   END IF
              END IF
!
              IF ( PIM%CONF%L_FIL ==  1 ) LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'INTMOD_TYPE:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__MOD_NO ) THEN
                   PIM%CONF%INTMOD_TYPE = PIMA__MOD_NO 
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__MOD_VERA1000 ) THEN
                   PIM%CONF%INTMOD_TYPE = PIMA__MOD_VERA1000
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__MOD_VERA2000 ) THEN
                   PIM%CONF%INTMOD_TYPE = PIMA__MOD_VERA2000
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__MOD_SFXC ) THEN
                   PIM%CONF%INTMOD_TYPE = PIMA__MOD_SFXC 
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__MOD_KJCC ) THEN
                   PIM%CONF%INTMOD_TYPE = PIMA__MOD_KJCC
                 ELSE 
                   CALL ERR_LOG ( 8129, IUER, 'PIMA_CONF', 'Unknown external '// &
     &                 'interferometric model type: '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' specified in the control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' in keywrod '//BUF(J3)(IND(1,1):IND(2,1))// &
     &                 ' supported types: '//PIMA__MOD_NO//' '// &
     &                 PIMA__MOD_VERA1000//' '//PIMA__MOD_VERA2000//' '// &
     &                 PIMA__MOD_SFXC//' '//PIMA__MOD_KJCC  )
                   RETURN
              END IF
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'INTMOD_FILE:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'NO' ) THEN
                   CONTINUE
                 ELSE
                   PIM%CONF%L_INM = PIM%CONF%L_INM + 1
                   IF ( PIM%CONF%L_INM > PIM__MFIL ) THEN
                        CALL CLRCH (            STR )
                        CALL INCH  ( PIM__MFIL, STR )
                        CALL ERR_LOG ( 8130, IUER, 'PIMA_CONF', 'Too many UV '// &
     &                      'input files specified in the control file '// &
     &                       CONF_FILE(1:I_LEN(CONF_FILE))//' more than the '// &
     &                      'limit PIM__MFIL '//STR )
                        RETURN
                    END IF
!
                    PIM%CONF%INTMOD_FILE(PIM%CONF%L_INM) = BUF(J3)(IND(1,2):IND(2,2))
                    INQUIRE ( FILE=PIM%CONF%INTMOD_FILE(PIM%CONF%L_INM), EXIST=LEX )
                    IF ( .NOT. LEX ) THEN
                         IP = I_LEN(PIM%CONF%INTMOD_FILE(PIM%CONF%L_INM))
                         CALL ERR_LOG ( 8131, IUER, 'PIMA_CONF', 'Cannot find '// &
     &                       'input apriori file '// &
     &                        PIM%CONF%INTMOD_FILE(PIM%CONF%L_INM)(1:IP)// &
     &                       ' specified in the control file '//CONF_FILE )
                         RETURN
                    END IF
              END IF
!
              IF ( PIM%CONF%L_INM .LE.  1 ) LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'TSYS:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__TSYS_NO ) THEN
                   PIM%CONF%TSYS_CAL_CODE = PIMA__TSYS_NO
                ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__TSYS_MEASURED .OR. &
     &                    BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__TSYS_INTRP         ) THEN
                   PIM%CONF%TSYS_CAL_CODE = PIMA__TSYS_MEASURED
                ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__TSYS_CLEANED ) THEN
                   PIM%CONF%TSYS_CAL_CODE = PIMA__TSYS_CLEANED
                ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__TSYS_MODELED ) THEN
                   PIM%CONF%TSYS_CAL_CODE = PIMA__TSYS_MODELED
                ELSE
                   CALL ERR_LOG ( 8132, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword TSYS' )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'PCAL:' ) THEN
              IP = INDEX ( BUF(J3)(IND(1,2):IND(2,2)), ':' )
!
! ----------- Check, whether PCAL keyword defines a list of not-to-use pcal stations
!
              PIM%CONF%L_PUS = 0
              IF ( IP < 1 ) THEN
                   PCAL_USE_STR = BUF(J3)(IND(1,2):IND(2,2))
                 ELSE
!
! ---------------- Yes, it has at least one station in the not-to-use list
!
                   PCAL_USE_STR = BUF(J3)(IND(1,2):IND(1,2)+IP-2)
                   STR2 = BUF(J3)(IND(1,2)+IP:)
!
! ---------------- Parse the station list and out its contents into PIM%CONF%PCAL_USE_STA(J4)
!
                   DO 440 J4=1,PIM__MSTA+1
                      IP = INDEX ( STR2, ':' )
                      IF ( IP < 1 ) IP = INDEX ( STR2, ',' )
                      IF ( IP > 0 ) THEN
                           STR = STR2(1:IP-1)
                           STR2 = STR2(IP+1:)
                         ELSE
                           STR = STR2
                      END IF
                      IF ( J4 == 1 ) THEN
                           CALL TRAN ( 11, STR, STR )
                           IF ( TRIM(STR) == 'TO_USE' ) THEN
                                PIM%CONF%PUS_TYPE = PIMA__USE
                              ELSE IF ( TRIM(STR) == 'NOT_TO_USE' ) THEN
                                PIM%CONF%PUS_TYPE = PIMA__NOT_USE
                              ELSE
                                CALL CLRCH (     STR1 )
                                CALL INCH  ( J3, STR1 )
                                CALL ERR_LOG ( 8133, IUER, 'PIMA_CONF', 'Error during '// &
     &                              'parsing the '//STR1(1:I_LEN(STR1))//'-th line of the '// &
     &                              'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                              ' -- keyword PCAL: -- qualifer TO_USE or NOT_TO_USE '// &
     &                              'was expected after column, but got '//STR )
                                RETURN
                           END IF
                           IF ( IP < 1 ) THEN
                                GOTO 840
                              ELSE
                                GOTO 440
                           END IF
                      END IF
                      PIM%CONF%L_PUS = PIM%CONF%L_PUS + 1
                      PIM%CONF%PCAL_USE_STA(PIM%CONF%L_PUS) = STR
!@                      IF ( PIM%NSTA > 0 ) THEN
!@                           IF ( LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, PIM%CONF%PCAL_NOTUSE_STA(J4) ) < 1 ) THEN
!@                                CALL ERR_LOG ( 8133, IUER, 'PIMA_CONF', 'Error during '// &
!@     &                              'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
!@     &                              'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
!@     &                              ' -- keyword PCAL: -- a not to use station '// &
!@     &                              PIM%CONF%PCAL_NOTUSE_STA(J4)//' was not observed '// &
!@     &                             'in VLBI experiment '//PIM%CONF%SESS_CODE )
!@                                RETURN
!@                           END IF
!@                      END IF
                      IF ( IP < 1 ) GOTO 840
  440              CONTINUE 
  840              CONTINUE 
              END IF
              CALL CHIN ( PCAL_USE_STR, NPCL )
              IF ( PCAL_USE_STR == PIMA__PCAL_NO(1:2) ) THEN
                   PIM%CONF%PHAS_CAL_CODE = PIMA__PCAL_NO
                ELSE IF ( PCAL_USE_STR == PIMA__PCAL_USE ) THEN
                   PIM%CONF%PHAS_CAL_CODE = PIMA__PCAL_USE
                ELSE IF ( PCAL_USE_STR == PIMA__PCAL_USE_ONE ) THEN
                   PIM%CONF%PHAS_CAL_CODE = PIMA__PCAL_USE_ONE
                ELSE IF ( PCAL_USE_STR == PIMA__PCAL_USE_ALL ) THEN
                   PIM%CONF%PHAS_CAL_CODE = PIMA__PCAL_USE_ALL
                ELSE IF ( NPCL .GE. 1 .AND. NPCL .LE. PIM__MTON ) THEN
                   PIM%CONF%PHAS_CAL_CODE = PCAL_USE_STR
                ELSE
                   CALL ERR_LOG ( 8133, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword PCAL PCAL_USE_STR: '//PCAL_USE_STR )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'GAIN:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__GAIN_NO ) THEN
                   PIM%CONF%GAIN_CAL_CODE = PIMA__GAIN_NO
                ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__GAIN_USE ) THEN
                   PIM%CONF%GAIN_CAL_CODE = PIMA__GAIN_USE
                ELSE
                   CALL ERR_LOG ( 8134, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword GAIN: USE or NO were expected' )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SAMPLER_CAL:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__SMPL_NO ) THEN
                   PIM%CONF%SAMPLER_CAL_CODE = PIMA__SMPL_NO
                ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__SMPL_USE ) THEN
                   PIM%CONF%SAMPLER_CAL_CODE = PIMA__SMPL_USE
                ELSE
                   CALL ERR_LOG ( 8135, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword SAMPLER_CAL' )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'EXPER_DIR:' ) THEN
              PIM%CONF%EXPER_DIR = BUF(J3)(IND(1,2):IND(2,2))
              DIR_DESC = FUNC_OPENDIR ( PIM%CONF%EXPER_DIR(1:I_LEN(PIM%CONF%EXPER_DIR))//CHAR(0) )
              IF ( DIR_DESC .EQ. 0 ) THEN
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 8136, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'attempt to read scratch directory '// &
     &                  PIM%CONF%EXPER_DIR(1:I_LEN(PIM%CONF%EXPER_DIR))// &
     &                 ' -- '//STR )
                   RETURN
                 ELSE
                   CALL CLOSEDIR ( %VAL(DIR_DESC) )
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'VTD_CONFIG_FILE:' ) THEN
              PIM%CONF%VTD_CONFIG_FILE = BUF(J3)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=PIM%CONF%VTD_CONFIG_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 8137, IUER, 'PIMA_CONF', 'Cannot find '// &
     &                 'input VTD configuration file '// &
     &                  PIM%CONF%VTD_CONFIG_FILE(1:I_LEN(PIM%CONF%VTD_CONFIG_FILE))// &
     &                 ' specified in the control file '//CONF_FILE )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'FFT_CONFIG_FILE:' ) THEN
              PIM%CONF%FFT_CONFIG_FILE = BUF(J3)(IND(1,2):IND(2,2))
              IF ( PIM%CONF%FFT_CONFIG_FILE .EQ. 'NO' ) THEN
                   CONTINUE
                ELSE
                   INQUIRE ( FILE=PIM%CONF%FFT_CONFIG_FILE, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 8138, IUER, 'PIMA_CONF', 'Cannot find '// &
     &                      'input FFT_CONFIG_FILE file '// &
     &                       PIM%CONF%FFT_CONFIG_FILE(1:I_LEN(PIM%CONF%FFT_CONFIG_FILE))// &
     &                      ' specified in the control file '//CONF_FILE )
                        RETURN
                   END IF
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'FFT_METHOD:' ) THEN
              CALL TRAN ( 11, BUF(J3)(IND(1,2):IND(2,2)), &
     &                        BUF(J3)(IND(1,2):IND(2,2)) )
              IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'ESTIMATE' ) THEN
                   PIM%CONF%FFT_METHOD = 0
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'MEASURE' ) THEN
                   PIM%CONF%FFT_METHOD = 1
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'PATIENT' ) THEN
                   PIM%CONF%FFT_METHOD = 2
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'EXHAUSTIVE' ) THEN
                   PIM%CONF%FFT_METHOD = 3
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'MKL' ) THEN
                   PIM%CONF%FFT_METHOD = FFT_MKL
                 ELSE
                   CALL ERR_LOG ( 8139, IUER, 'PIMA_CONF', 'Wrong value '// &
     &                 'of the value of keyword FFT_METHOD: '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))//' one of MKL, ESTIMATE, '// &
     &                 'MEASURE, PATIENT and EXHAUSTIVE was expected' )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'NUM_THREADS:' ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I4)', IOSTAT=IER ) &
     &               PIM%CONF%NUM_THREADS
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8140, IUER, 'PIMA_CONF', 'Wrong value '// &
     &                 'of the keyword NUM_THREADS: '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))//' an integer in range '//&
     &                 ' [1, 8192] was expected' )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'UV_EXCLUDE_FILE:' ) THEN
              PIM%CONF%EXCLUDE_UV_FINAM = BUF(J3)(IND(1,2):IND(2,2))
              IF ( PIM%CONF%EXCLUDE_UV_FINAM == PIMA__EXC_NO ) THEN
                   CONTINUE
                 ELSE IF ( PIM%CONF%EXCLUDE_UV_FINAM == PIMA__EXC_AUTO ) THEN
                   CONTINUE
                 ELSE IF ( PIM%CONF%EXCLUDE_UV_FINAM(1:2) .NE. 'NO' ) THEN
                   INQUIRE ( FILE=PIM%CONF%EXCLUDE_UV_FINAM, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 8141, IUER, 'PIMA_CONF', 'Cannot find '// &
     &                     'input UV_EXCLUDE_FILE file '// &
     &                      PIM%CONF%EXCLUDE_UV_FINAM(1:I_LEN(PIM%CONF%EXCLUDE_UV_FINAM))// &
     &                     ' specified in the control file '//CONF_FILE )
                        RETURN
                   END IF
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'BANDPASS_USE:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__BPASS_NO ) THEN
                   PIM%CONF%BANDPASS_USE = PIMA__BPASS_NO
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__BPASS_AMP ) THEN
                   PIM%CONF%BANDPASS_USE = PIMA__BPASS_AMP
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__BPASS_PHS ) THEN
                   PIM%CONF%BANDPASS_USE = PIMA__BPASS_PHS
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__BPASS_AMP_PHS ) THEN
                   PIM%CONF%BANDPASS_USE = PIMA__BPASS_AMP_PHS
                 ELSE
                   CALL ERR_LOG ( 8142, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword BANDPASS_USE' )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRINGE_FILE:' ) THEN
              PIM%CONF%FRINGE_FILE = BUF(J3)(IND(1,2):IND(2,2))
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIRES_FILE:' ) THEN
              PIM%CONF%FRIRES_FILE = BUF(J3)(IND(1,2):IND(2,2))
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'INCLUDE_OBS_FILE:' ) THEN
              PIM%CONF%INCLUDE_OBS_FILE = BUF(J3)(IND(1,2):IND(2,2))
              IF ( PIM%CONF%INCLUDE_OBS_FILE .EQ. 'NO' ) THEN
                   CONTINUE
                 ELSE
                   INQUIRE ( FILE=PIM%CONF%INCLUDE_OBS_FILE, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 8143, IUER, 'PIMA_CONF', 'Cannot '// &
     &                      'find input observations include file '// &
     &                       PIM%CONF%INCLUDE_OBS_FILE(1:I_LEN(PIM%CONF%INCLUDE_OBS_FILE))// &
     &                      ' specified in the control file '//CONF_FILE )
                        RETURN
                   END IF
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'EXCLUDE_OBS_FILE:' ) THEN
              PIM%CONF%EXCLUDE_OBS_FILE = BUF(J3)(IND(1,2):IND(2,2))
              IF ( PIM%CONF%EXCLUDE_OBS_FILE .EQ. 'NO' ) THEN
                   CONTINUE
                 ELSE
                   INQUIRE ( FILE=PIM%CONF%EXCLUDE_OBS_FILE, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 8144, IUER, 'PIMA_CONF', 'Cannot '// &
     &                      'find input observations exclude file '// &
     &                      PIM%CONF%EXCLUDE_OBS_FILE(1:I_LEN(PIM%CONF%EXCLUDE_OBS_FILE))// &
     &                     ' specified in the control file '//CONF_FILE )
                        RETURN
                   END IF
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'BANDPASS_FILE:' ) THEN
              PIM%CONF%BANDPASS_FILE = BUF(J3)(IND(1,2):IND(2,2))
              IF ( PIM%CONF%BANDPASS_FILE .EQ. 'NO' ) THEN
                   PIM%CONF%BANDPASS_USE = PIMA__BPASS_NO
                   IF ( PIM%CONF%ACT_CODE == PIMA__BPLT_CODE ) THEN
                        CALL ERR_LOG ( 8145, IUER, 'PIMA_CONF', 'Bandpass '// &
     &                      'file name is not specified. It should be specified '// &
     &                      'when opeation bplt is used. Please specify the name '// &
     &                      'of the bandpass file in keyword '// &
     &                      'BANDPASS_FILE of the control file '//CONF_FILE )
                        RETURN 
                   END IF
                 ELSE
                   IF ( PIM%CONF%ACT_CODE .NE. PIMA__BPAS_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__GEAN_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__BMGE_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__PMGE_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__TSPL_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__PRGA_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__PCPL_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__PDPL_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__MPPL_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__OPAG_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__OPAL_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__ACPL_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__MOIM_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__GEPM_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__CLPC_CODE .AND. &
     &                  .NOT. ( PIM%CONF%ACT_CODE .EQ. PIMA__GEAN_CODE .AND. &
     &                          KEYWORD(1) == 'wvr'                          ) .AND. &
     &                  .NOT. ( PIM%CONF%ACT_CODE .EQ. PIMA__GEAN_CODE .AND. &
     &                          KEYWORD(1)(1:4) == 'pcal'                    ) ) THEN
                        INQUIRE ( FILE=PIM%CONF%BANDPASS_FILE, EXIST=LEX )
                        IF ( .NOT. LEX ) THEN
                             CALL ERR_LOG ( 8146, IUER, 'PIMA_CONF', 'Cannot '// &
     &                           'find input BANDPASS_FILE file '// &
     &                           PIM%CONF%BANDPASS_FILE(1:I_LEN(PIM%CONF%BANDPASS_FILE))// &
     &                          ' specified in the control file '//CONF_FILE )
                             RETURN
                        END IF
                   END IF
                   IF ( PIM%CONF%ACT_CODE .EQ. PIMA__TSPL_CODE .OR. &
     &                  PIM%CONF%ACT_CODE .EQ. PIMA__BMGE_CODE .OR. &
     &                  PIM%CONF%ACT_CODE .EQ. PIMA__PMGE_CODE .OR. &
     &                  PIM%CONF%ACT_CODE .EQ. PIMA__ACPL_CODE .OR. &
     &                  PIM%CONF%ACT_CODE .EQ. PIMA__PCPL_CODE .OR. &
     &                  PIM%CONF%ACT_CODE .EQ. PIMA__PDPL_CODE .OR. &
     &                  ( PIM%CONF%ACT_CODE .NE. PIMA__MPPL_CODE .AND. &
     &                     PIM%CONF%ACT_CODE .EQ. PIMA__ACPL_CODE      ) .OR. &
     &                  ( PIM%CONF%ACT_CODE .EQ. PIMA__GEAN_CODE .AND. &
     &                    KEYWORD(1) == 'wvr'                          ) ) THEN
                        PIM%CONF%BANDPASS_FILE = PIMA__BPASS_NO
                   END IF
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'POLARCAL_FILE:' ) THEN
              PIM%CONF%POLARCAL_FILE = BUF(J3)(IND(1,2):IND(2,2))
              IF ( PIM%CONF%POLARCAL_FILE .EQ. 'NO' ) THEN
                   PIM%CONF%POLARCAL_FILE = PIMA__POLARCAL_NO
                 ELSE
                   CONTINUE 
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'BANDPASS_MASK_FILE:' ) THEN
              PIM%CONF%BANDPASS_MASK_FILE = BUF(J3)(IND(1,2):IND(2,2))
              IF ( PIM%CONF%BANDPASS_MASK_FILE .EQ. 'NO' ) THEN
                   PIM%CONF%BANDPASS_MASK_FILE = PIMA__BPASS_NO
                   IF ( PIM%CONF%ACT_CODE .EQ. PIMA__BMGE_CODE       ) THEN
                        CALL ERR_LOG ( 8147, IUER, 'PIMA_CONF', 'Bandpass mask '// &
     &                      'file name is not specified. It should be specified '// &
     &                      'when opeation bmge is used. Please specify the name '// &
     &                      'of the bandpass mask file in keyword '// &
     &                      'BANDPASS_MASK_FILE of the control file '//CONF_FILE )
                        RETURN 
                   END IF
                 ELSE
                   IF ( PIM%CONF%ACT_CODE .NE. PIMA__GEAN_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__BMGE_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__PMGE_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__TSPL_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__PCPL_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__PDPL_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__MPPL_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__OPAG_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__OPAL_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__MOIM_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__GEPM_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__CLPC_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__ACPL_CODE       ) THEN
                        INQUIRE ( FILE=PIM%CONF%BANDPASS_MASK_FILE, EXIST=LEX )
                        IF ( .NOT. LEX ) THEN
                             CALL ERR_LOG ( 8148, IUER, 'PIMA_CONF', 'Cannot '// &
     &                           'find input BANDPASS_MASK_FILE file '// &
     &                           PIM%CONF%BANDPASS_MASK_FILE(1:I_LEN(PIM%CONF%BANDPASS_MASK_FILE))// &
     &                          ' specified in the control file '//CONF_FILE )
                             RETURN
                        END IF
                   END IF
                   IF ( PIM%CONF%ACT_CODE .EQ. PIMA__GEAN_CODE .OR. &
     &                  PIM%CONF%ACT_CODE .EQ. PIMA__TSPL_CODE .OR. &
     &                  PIM%CONF%ACT_CODE .EQ. PIMA__PCPL_CODE .OR. &
     &                  PIM%CONF%ACT_CODE .EQ. PIMA__PDPL_CODE .OR. &
     &                  PIM%CONF%ACT_CODE .EQ. PIMA__MPPL_CODE      ) THEN
                        PIM%CONF%BANDPASS_MASK_FILE = PIMA__BPASS_NO
                      ELSE IF ( PIM%CONF%ACT_CODE .EQ. PIMA__ACPL_CODE       ) THEN
                        INQUIRE ( FILE=PIM%CONF%BANDPASS_MASK_FILE, EXIST=LEX )
                        IF ( .NOT. LEX ) THEN
                             IF ( PIM%CONF%WARNING ) THEN
                                  WRITE ( 6, '(A)' ) 'PIMA: Warning: bandpass mask file '//TRIM(PIM%CONF%BANDPASS_MASK_FILE)// &
     &                                               ' -- continue with disabling the bandpass mask' 
                             END IF
                             PIM%CONF%BANDPASS_MASK_FILE = PIMA__BPASS_NO
                        END IF
                   END IF
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'PCAL_MASK_FILE:' ) THEN
              PIM%CONF%PCAL_MASK_FILE = BUF(J3)(IND(1,2):IND(2,2))
              IF ( PIM%CONF%PCAL_MASK_FILE .EQ. 'NO' ) THEN
                   PIM%CONF%PCAL_MASK_FILE = PIMA__BPASS_NO
                   IF ( PIM%CONF%ACT_CODE .EQ. PIMA__PMGE_CODE .OR. &
     &                  PIM%CONF%ACT_CODE .EQ. PIMA__GEPM_CODE      ) THEN
                        CALL ERR_LOG ( 8149, IUER, 'PIMA_CONF', 'Phase-cal mask '// &
     &                      'file name is not specified. It should be specified '// &
     &                      'when operation pmge or gepm is used. Please specify the '// &
     &                      'name of the phase-cal mask file in keyword '// &
     &                      'PCAL_MASK_FILE of the control file '//CONF_FILE )
                        RETURN 
                   END IF
                 ELSE
                   IF ( PIM%CONF%ACT_CODE .NE. PIMA__GEAN_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__PMGE_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__BMGE_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__TSPL_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__PCPL_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__PDPL_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__MOIM_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__GEPM_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__CLPC_CODE .AND. &
     &                  PIM%CONF%ACT_CODE .NE. PIMA__ACPL_CODE       ) THEN
                        INQUIRE ( FILE=PIM%CONF%PCAL_MASK_FILE, EXIST=LEX )
                        IF ( .NOT. LEX ) THEN
                             CALL ERR_LOG ( 8150, IUER, 'PIMA_CONF', 'Cannot '// &
     &                           'find input PCAL_MASK_FILE file '// &
     &                           PIM%CONF%PCAL_MASK_FILE(1:I_LEN(PIM%CONF%PCAL_MASK_FILE))// &
     &                          ' specified in the control file '//CONF_FILE )
                             RETURN
                        END IF
!
                      ELSE IF ( PIM%CONF%ACT_CODE .EQ. PIMA__GEPM_CODE ) THEN
                        IF ( .NOT. FL_KEY_OVER ) THEN
!
! -------------------------- GEPM task. If no-overwrite mode was requersted,
! -------------------------- whether the pcal bandpass mask exists. 
! -------------------------- IF yes, stop
!
                             INQUIRE ( FILE=PIM%CONF%PCAL_MASK_FILE, EXIST=LEX )
                             IF ( LEX ) THEN
                                  CALL ERR_LOG ( 8151, IUER, 'PIMA_CONF', 'Found '// &
     &                                'PCAL_MASK_FILE file '//TRIM(PIM%CONF%PCAL_MASK_FILE)// &
     &                                ' specified in the control file '//TRIM(CONF_FILE)// &
     &                                '. Please specity overwrite yes if you want to '// &
     &                                'overwrite the existing phase cal mask' )
                                  RETURN
                             END IF
                        END IF
                   END IF
                   IF ( PIM%CONF%ACT_CODE .EQ. PIMA__GEAN_CODE .OR. &
     &                  PIM%CONF%ACT_CODE .EQ. PIMA__TSPL_CODE .OR. &
     &                  PIM%CONF%ACT_CODE .EQ. PIMA__PCPL_CODE .OR. &
     &                  PIM%CONF%ACT_CODE .EQ. PIMA__PDPL_CODE .OR. &
     &                  PIM%CONF%ACT_CODE .EQ. PIMA__ACPL_CODE       ) THEN
                        PIM%CONF%PCAL_MASK_FILE = PIMA__BPASS_NO
                   END IF
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'CORR_FLAG_MIN:' ) THEN
              CALL CHIN ( BUF(J3)(IND(1,2):IND(2,2)), PIM%CONF%CORR_FLAG_MIN )
              IF ( PIM%CONF%CORR_FLAG_MIN < -999 .OR. &
     &             PIM%CONF%CORR_FLAG_MIN >  999      ) THEN
!
                   CALL ERR_LOG ( 8151, IUER, 'PIMA_CONF', 'Wrong value '// &
     &                 'of CORR_FLAG_MIN: '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- should be in the range [-999, 999] '// &
     &                 ' please correct the control file '//CONF_FILE )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'TIME_FLAG_FILE:' ) THEN
              PIM%CONF%TIME_FLAG_FILE = BUF(J3)(IND(1,2):IND(2,2))
              IF ( PIM%CONF%TIME_FLAG_FILE .EQ. 'NO' ) THEN
                   CALL CLRCH ( PIM%CONF%TIME_FLAG_FILE )
                   IF ( PIM%CONF%ACT_CODE == PIMA__ONOF_CODE ) THEN
                        CALL ERR_LOG ( 8152, IUER, 'PIMA_CONF', 'File '// &
     &                      'name for option PIM%CONF%TIME_FLAG_FILE '// &
     &                      'should be specified in order task ONOF '// &
     &                      'could write results' )
                        RETURN
                   END IF
                 ELSE
                    IF ( PIM%CONF%ACT_CODE .NE. PIMA__ONOF_CODE .AND. &
     &                   PIM%CONF%ACT_CODE .NE. PIMA__PCPL_CODE .AND. &
     &                   PIM%CONF%ACT_CODE .NE. PIMA__MPPL_CODE .AND. &
     &                   PIM%CONF%ACT_CODE .NE. PIMA__BPLT_CODE .AND. &
     &                   PIM%CONF%ACT_CODE .NE. PIMA__PPLT_CODE .AND. &
     &                   PIM%CONF%ACT_CODE .NE. PIMA__BMGE_CODE .AND. &
     &                   PIM%CONF%ACT_CODE .NE. PIMA__PMGE_CODE .AND. &
     &                   PIM%CONF%ACT_CODE .NE. PIMA__PRGA_CODE .AND. &
     &                   PIM%CONF%ACT_CODE .NE. PIMA__GEAN_CODE .AND. &
     &                   PIM%CONF%ACT_CODE .NE. PIMA__GEPM_CODE .AND. &
     &                   PIM%CONF%ACT_CODE .NE. PIMA__MKDB_CODE .AND. &
     &                   PIM%CONF%ACT_CODE .NE. PIMA__OPAG_CODE .AND. &
     &                   PIM%CONF%ACT_CODE .NE. PIMA__OPAL_CODE .AND. &
     &                   PIM%CONF%ACT_CODE .NE. PIMA__MOIM_CODE .AND. &
     &                   PIM%CONF%ACT_CODE .NE. PIMA__CLPC_CODE .AND. &
     &                   PIM%CONF%ACT_CODE .NE. PIMA__TSMO_CODE .AND. &
     &                   PIM%CONF%ACT_CODE .NE. PIMA__TSPL_CODE       ) THEN
                         INQUIRE ( FILE=PIM%CONF%TIME_FLAG_FILE, EXIST=LEX      )
                         IF ( .NOT. LEX ) THEN
                              CALL ERR_LOG ( 8153, IUER, 'PIMA_CONF', 'Cannot '// &
     &                            'find input PIM%CONF%TIME_FLAG_FILE file '// &
     &                             PIM%CONF%TIME_FLAG_FILE(1:I_LEN(PIM%CONF%TIME_FLAG_FILE))// &
     &                            ' specified in the control file '//CONF_FILE )
                              RETURN
                         END IF
                   END IF
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'TEC_FILE:' ) THEN
              PIM%CONF%TEC_FILE = BUF(J3)(IND(1,2):IND(2,2))
              IF ( PIM%CONF%TEC_FILE .EQ. 'NO' ) THEN
                   CALL CLRCH ( PIM%CONF%TEC_FILE )
                   IF ( PIM%CONF%ACT_CODE == PIMA__TECG_CODE ) THEN
                        CALL ERR_LOG ( 8154, IUER, 'PIMA_CONF', 'File '// &
     &                      'name for option PIM%CONF%TEC_FILE '// &
     &                      'should be specified in order task TECG '// &
     &                      'could write results' )
                        RETURN
                   END IF
                 ELSE
                   IF ( PIM%CONF%ACT_CODE .NE. PIMA__TECG_CODE ) THEN
                        INQUIRE ( FILE=PIM%CONF%TEC_FILE, EXIST=LEX      )
                        IF ( .NOT. LEX ) THEN
                             CALL ERR_LOG ( 8155, IUER, 'PIMA_CONF', 'Cannot '// &
     &                           'find input PIM%CONF%TEC_FILE file '// &
     &                            PIM%CONF%TEC_FILE(1:I_LEN(PIM%CONF%TEC_FILE))// &
     &                           ' specified in the control file '//CONF_FILE )
                             RETURN
                        END IF
                   END IF
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'WVR_FILE:' ) THEN
              PIM%CONF%L_WVR = PIM%CONF%L_WVR + 1
              IF ( PIM%CONF%L_WVR == 1 ) LKEY = LKEY + 1
              IF ( PIM%CONF%L_WVR > PIM__MWVR ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( PIM__MWVR, STR )
                   CALL ERR_LOG ( 8156, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the control file '// &
     &                  CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- too many WVR files: more than PIM__MWVR='//STR )
                   RETURN
              END IF
!
              PIM%CONF%WVR_FILE(PIM%CONF%L_WVR) = BUF(J3)(IND(1,2):IND(2,2))
              IF ( PIM%CONF%WVR_FILE(PIM%CONF%L_WVR) .EQ. 'NO' ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   IF ( PIM%CONF%L_WVR > 1 ) THEN
                        CALL ERR_LOG ( 8157, IUER, 'PIMA_CONF', 'Failure in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line of '// &
     &                      'the control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                      ' -- since there were WVR files previosly defined, '// &
     &                      'value NO is not acceptable' )
                        RETURN
                   END IF
                   PIM%CONF%L_WVR = 0
                 ELSE
                   INQUIRE ( FILE=PIM%CONF%WVR_FILE(PIM%CONF%L_WVR), EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 8158, IUER, 'PIMA_CONF', 'Cannot '// &
     &                      'find input WVR_FILE file '// &
     &                       PIM%CONF%WVR_FILE(PIM%CONF%L_WVR)(1:I_LEN(PIM%CONF%WVR_FILE(PIM%CONF%L_WVR)))// &
     &                      ' specified in the control file '//CONF_FILE )
                        RETURN
                   END IF
              END IF
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'WVR_USE:' ) THEN
              PIM%CONF%WVR_USE = BUF(J3)(IND(1,2):IND(2,2))
              IF ( PIM%CONF%WVR_USE == PIMA__WVR_NO ) THEN
                   CONTINUE 
                 ELSE IF ( PIM%CONF%WVR_USE == PIMA__WVR_SPLINE_3RD ) THEN
                   CONTINUE 
                 ELSE IF ( PIM%CONF%WVR_USE == PIMA__WVR_SPLINE_LIN ) THEN
                   CONTINUE 
                 ELSE IF ( PIM%CONF%WVR_USE == PIMA__WVR_SPLINE_AVR ) THEN
                   CONTINUE 
                 ELSE
                   CALL ERR_LOG ( 8159, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword WVR_USE. Values NO, WVR_3SPL, '// &
     &                 'WVR_LIN, WVR_AVR were expected' )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'WVR_SMOOTHING_INTERVAL:' ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F10.5)', IOSTAT=IER ) &
     &               PIM%CONF%WVR_SMOOTHING_INTERVAL
              IF ( IER .NE. 0 .OR. PIM%CONF%WVR_SMOOTHING_INTERVAL < 0.0D0 ) THEN
                   CALL ERR_LOG ( 8160, IUER, 'PIMA_CONF', 'Wrong value '// &
     &                 'of the keyword WVR_SMOOTHING_INTERVAL: a non-negative '// &
     &                 'real number was expected' )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'WVR_SMOOTHING_SIGMA:' ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F10.5)', IOSTAT=IER ) &
     &               PIM%CONF%WVR_SMOOTHING_SIGMA
              IF ( IER .NE. 0 .OR. PIM%CONF%WVR_SMOOTHING_INTERVAL < 0.0D0 ) THEN
                   CALL ERR_LOG ( 8161, IUER, 'PIMA_CONF', 'Wrong value '// &
     &                 'of the keyword WVR_SMOOTHING_SIGMA: a non-negative '// &
     &                 'real number was expected' )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'WVR_SMOOTHING_SIGMA:' ) THEN
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BEG_FRQ:' .OR. &
     &                BUF(J3)(IND(1,1):IND(2,1)) == 'FRQ_BEG:'      ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I4)', IOSTAT=IER ) &
     &               PIM%CONF%BEG_FRQ
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8162, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword BEG_FRQ' )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'END_FRQ:' .OR. &
     &                BUF(J3)(IND(1,1):IND(2,1)) == 'FRQ_END:'      ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I4)', IOSTAT=IER ) &
     &               PIM%CONF%END_FRQ
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8163, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword END_FRQ' )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'POLAR:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_RR ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_RR
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_LL ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_LL
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_RL ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_RL
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_LR ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_LR
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_HH ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_HH
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_HV ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_HV
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_VV ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_VV
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_VH ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_VH
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_XX ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_XX
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_XY ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_XY
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_YX ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_YX
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_YY ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_YY
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_HR ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_HR
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_RH ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_RH
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_HL ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_HL
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_LH ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_LH
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_VR ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_VR
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_RV ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_RV
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_VL ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_VL
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_LV ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_LV
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_I  ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_I
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_Q  ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_Q
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_U  ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_U
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_V  ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_V
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_ALL ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_ALL
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_ORIG ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_ORIG
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_1ST ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_1ST
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_2ND ) THEN
                   PIM%CONF%POLAR = PIMA__POLAR_2ND
                 ELSE
                   CALL ERR_LOG ( 8164, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong value of the keyword FRIB.POLAR.  '// &
     &                 'Code: '//BUF(J3)(IND(1,2):IND(1,2)+7)//' is not supported. '// &
     &                 ' Supported codes: RR, RL, LR, LL, I, Q, U, V, '// &
     &                 'HH, HV, VV, VH, XX, XY, YX, YY, ALL, ORIG, ALL_1ST, ALL_2ND' )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRQ_GRP:' ) THEN
              IP = INDEX ( BUF(J3)(IND(1,2):IND(2,2)), ':' ) 
              IH = INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '-' ) 
              IF ( IP .LE. 1 .AND. IH .LE. 1 ) THEN
                   READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I4)', IOSTAT=IER ) &
     &                    PIM%CONF%FRQ_GRP
                   PIM%VIRT_NFRG = 1
                   PIM%CONF%FRG_USE = PIMA__SINGLE
                 ELSE IF ( IP > 1 ) THEN
                   STR = BUF(J3)(IND(1,2):IND(2,2))
                   READ ( UNIT=STR(1:IP-1), FMT='(I4)', IOSTAT=IER ) &
     &                    PIM%CONF%FRG_LIST(1) 
                   PIM%CONF%FRQ_GRP = PIM%CONF%FRG_LIST(1) 
                   IF ( IER == 0 .AND. IP < ILEN(STR) )  THEN
                   READ ( UNIT=STR(IP+1:ILEN(STR)), FMT='(I4)', IOSTAT=IER ) &
     &                    PIM%CONF%FRG_LIST(2) 
                   END IF 
                   PIM%CONF%FRG_USE = PIMA__MERGE
                 ELSE IF ( IH > 1 ) THEN
                   STR = BUF(J3)(IND(1,2):IND(2,2))
                   READ ( UNIT=STR(1:IH-1), FMT='(I4)', IOSTAT=IER ) &
     &                    PIM%CONF%FRG_LIST(1) 
                   PIM%CONF%FRQ_GRP = PIM%CONF%FRG_LIST(1) 
                   IF ( IER == 0 .AND. IH < ILEN(STR) )  THEN
                   READ ( UNIT=STR(IH+1:ILEN(STR)), FMT='(I4)', IOSTAT=IER ) &
     &                    PIM%CONF%FRG_LIST(2) 
                   END IF 
                   PIM%CONF%FRG_USE = PIMA__COMBINE
              END IF
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8165, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.FRQ_GRP' )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'EPHEMERIDES_FILE:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'NO' ) THEN
                   CALL CLRCH ( PIM%CONF%EPHEMERIDES_FILE )
                 ELSE 
                   PIM%CONF%EPHEMERIDES_FILE = BUF(J3)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=PIM%CONF%EPHEMERIDES_FILE, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 8166, IUER, 'PIMA_CONF', 'Cannot '// &
     &                      'find input EPHEMERIDES_FILE file '// &
     &                      PIM%CONF%EPHEMERIDES_FILE(1:I_LEN(PIM%CONF%EPHEMERIDES_FILE))// &
     &                     ' specified in the control file '//CONF_FILE )
                        RETURN
                   END IF
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'EPHEMERIDES_USE:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'NO' ) THEN
                   PIM%CONF%EPHEMERIDES_USE = 'NO'
                 ELSE 
                   PIM%CONF%EPHEMERIDES_USE = BUF(J3)(IND(1,2):IND(2,2))
                   IF ( PIM%CONF%EPHEMERIDES_USE == 'NO' ) THEN
                        CONTINUE 
                      ELSE IF ( PIM%CONF%EPHEMERIDES_USE == 'INTERPLA' ) THEN
                        CONTINUE 
                      ELSE IF ( PIM%CONF%EPHEMERIDES_USE == 'EARTH_OR' ) THEN
                        CONTINUE 
                      ELSE IF ( PIM%CONF%EPHEMERIDES_USE == 'STA_ORB ' ) THEN
                        CONTINUE 
                      ELSE IF ( PIM%CONF%EPHEMERIDES_USE == 'RA_PUSCH' ) THEN
                        CONTINUE 
                      ELSE IF ( PIM%CONF%EPHEMERIDES_USE == 'RA_GBT  ' ) THEN
                        CONTINUE 
                      ELSE IF ( PIM%CONF%EPHEMERIDES_USE == 'RA_GB140' ) THEN
                        CONTINUE 
                      ELSE 
                        CALL ERR_LOG ( 8167, IUER, 'PIMA_CONF', 'Unsupported value '// &
     &                      'EPHEMERIDES_USE keyword: '// &
     &                      PIM%CONF%EPHEMERIDES_USE(1:I_LEN(PIM%CONF%EPHEMERIDES_USE))// &
     &                     ' while NO, or EARTH_OR, or STA_ORB, or RA_PUSCH, or RA_GBT, '// &
     &                     'or RA_GB140 were expected' )
                        RETURN
                   END IF
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'PHASE_ACCELERATION:' ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F16.6)', IOSTAT=IER ) &
     &               PIM%CONF%PHASE_ACCELERATION
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8168, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword PHASE_ACCELERATION' )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'PHASE_ACCEL_MIN:' ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F16.6)', IOSTAT=IER ) &
     &               PIM%CONF%PHASE_ACCEL_MIN
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8169, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword PHASE_ACCEL_MIN' )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'PHASE_ACCEL_MAX:' ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F16.6)', IOSTAT=IER ) &
     &               PIM%CONF%PHASE_ACCEL_MAX
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8170, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword PHASE_ACCEL_MAX' )
                   RETURN
              END IF
              LKEY = LKEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.SEARCH_TYPE:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__2FFT ) THEN
                   PIM%CONF%FRIB_SEARCH_TYPE = PIMA__2FFT
                 ELSE
                   CALL ERR_LOG ( 8171, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.SEARCH_TYPE' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.DELAY_WINDOW_CENTER:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F20.12)', IOSTAT=IER ) &
     &               PIM%CONF%FRIB_DELAY_WINDOW_CENTER
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8172, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.DELAY_WINDOW_CENTER' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.RATE_WINDOW_CENTER:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F20.12)', IOSTAT=IER ) &
     &               PIM%CONF%FRIB_RATE_WINDOW_CENTER
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8173, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.RATE_WINDOW_CENTER' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.DELAY_WINDOW_WIDTH:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F20.12)', IOSTAT=IER ) &
     &               PIM%CONF%FRIB_DELAY_WINDOW_WIDTH
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8174, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.DELAY_WINDOW_WIDTH' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.RATE_WINDOW_WIDTH:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F20.12)', IOSTAT=IER ) &
     &               PIM%CONF%FRIB_RATE_WINDOW_WIDTH
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8175, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.RATE_WINDOW_WIDTH' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.AUTOCORR_CALIB:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__ACCR_NO ) THEN
                   PIM%CONF%FRIB_AUTOCORR_CALIB = PIMA__ACCR_NO
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__ACCR_SQRT_MEA ) THEN
                   PIM%CONF%FRIB_AUTOCORR_CALIB = PIMA__ACCR_SQRT_MEA
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__ACCR_SQRT_EVR ) THEN
                   PIM%CONF%FRIB_AUTOCORR_CALIB = PIMA__ACCR_SQRT_EVR
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__ACCR_SQRT_KOG ) THEN
                   PIM%CONF%FRIB_AUTOCORR_CALIB = PIMA__ACCR_SQRT_KOG
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__ACCR_INTG ) THEN
                   PIM%CONF%FRIB_AUTOCORR_CALIB = PIMA__ACCR_INTG
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__ACCR_VLBA_CNST1 ) THEN
                   PIM%CONF%FRIB_AUTOCORR_CALIB = PIMA__ACCR_VLBA_CNST1
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__ACCR_VLBA_CNST2 ) THEN
                   PIM%CONF%FRIB_AUTOCORR_CALIB = PIMA__ACCR_VLBA_CNST2
                 ELSE
                   CALL ERR_LOG ( 8176, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.ACCR_CALIB' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.AMPL_FUDGE_TYPE:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__FUDGE_NO ) THEN
                   PIM%CONF%FRIB_AMPL_FUDGE_TYPE = PIMA__FUDGE_NO
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__FUDGE_MITA ) THEN
                   PIM%CONF%FRIB_AMPL_FUDGE_TYPE = PIMA__FUDGE_MITA
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__FUDGE_DWIN ) THEN
                   PIM%CONF%FRIB_AMPL_FUDGE_TYPE = PIMA__FUDGE_DWIN
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__FUDGE_VLBA ) THEN
                   PIM%CONF%FRIB_AMPL_FUDGE_TYPE = PIMA__FUDGE_VLBA
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__FUDGE_DIFX ) THEN
                   PIM%CONF%FRIB_AMPL_FUDGE_TYPE = PIMA__FUDGE_DIFX
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__FUDGE_KOGAN ) THEN
                   PIM%CONF%FRIB_AMPL_FUDGE_TYPE = PIMA__FUDGE_KOGAN
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == "VLBA_KOGAN" ) THEN
                   PIM%CONF%FRIB_AMPL_FUDGE_TYPE = PIMA__FUDGE_KOGAN
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__FUDGE_AIPS ) THEN
                   PIM%CONF%FRIB_AMPL_FUDGE_TYPE = PIMA__FUDGE_VLBA
                 ELSE
                   CALL ERR_LOG ( 8177, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.AMPL_FUDGE_TYPE' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.AMPL_EDGE_WINDOW_COR:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__EDGE_AMP_NO ) THEN
                   PIM%CONF%FRIB_AMPL_EDGE_WINDOW_COR = PIMA__EDGE_AMP_NO
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__EDGE_AMP_USE ) THEN
                   PIM%CONF%FRIB_AMPL_EDGE_WINDOW_COR = PIMA__EDGE_AMP_USE
                 ELSE
                   CALL ERR_LOG ( 8178, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of FRIB.AMPL_EDGE_WINDOW_COR. Supported values: NO or USE' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.AMPL_EDGE_BEAM_COR:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__BEAM_NO ) THEN
                   PIM%CONF%FRIB_AMPL_EDGE_BEAM_COR = PIMA__BEAM_NO
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__BEAM_YES ) THEN
                   PIM%CONF%FRIB_AMPL_EDGE_BEAM_COR = PIMA__BEAM_YES
                 ELSE
                   CALL ERR_LOG ( 8179, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of FRIB.AMPL_EDGE_BEAM_COR. Supported values: NO or YES' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.OVERSAMPLE_MD:' ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I4)', IOSTAT=IER ) &
     &               PIM%CONF%FRIB_OVERSAMPLE_MD
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8180, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.OVERSAMPLE_MD' )
                   RETURN
              END IF
              IF ( PIM%CONF%FRIB_OVERSAMPLE_MD < 1 ) THEN
                   CALL ERR_LOG ( 8181, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong '//BUF(J3)(IND(1,1):IND(2,1))//' '// &
     &                 BUF(J3)(IND(1,2):IND(2,2))//' an integer > 0 '// &
     &                 'is expected' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.OVERSAMPLE_RT:' ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I4)', IOSTAT=IER ) &
     &               PIM%CONF%FRIB_OVERSAMPLE_RT
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8182, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.OVERSAMPLE_RT' )
                   RETURN
              END IF
              IF ( PIM%CONF%FRIB_OVERSAMPLE_MD < 1 ) THEN
                   CALL ERR_LOG ( 8183, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong '//BUF(J3)(IND(1,1):IND(2,1))//' '// &
     &                 BUF(J3)(IND(1,2):IND(2,2))//' an integer > 0 '// &
     &                 'is expected' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.FINE_SEARCH:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'NO' ) THEN
                   PIM%CONF%FRIB_FINE_SEARCH = PIMA__FINE_SEARCH_NO
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'PAR' ) THEN
                   PIM%CONF%FRIB_FINE_SEARCH = PIMA__FINE_SEARCH_PAR
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'BIN' ) THEN
                   PIM%CONF%FRIB_FINE_SEARCH = PIMA__FINE_SEARCH_BIN
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'LSQ' ) THEN
                   PIM%CONF%FRIB_FINE_SEARCH = PIMA__FINE_SEARCH_LSQ
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'ACC' ) THEN
                   PIM%CONF%FRIB_FINE_SEARCH = PIMA__FINE_SEARCH_ACC
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'TEC' ) THEN
                   PIM%CONF%FRIB_FINE_SEARCH = PIMA__FINE_SEARCH_TEC
                 ELSE
                   CALL ERR_LOG ( 8184, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unsupported value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.FINE_SEARCH' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.AUTOCORR_THRESHOLD:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F20.12)', IOSTAT=IER ) &
     &               PIM%CONF%FRIB_AUTOCORR_THRESHOLD
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8185, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.AUTOCORR_THRESHOLD' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.WEIGHTS_THRESHOLD:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F20.12)', IOSTAT=IER ) &
     &               PIM%CONF%FRIB_WEIGHTS_THRESHOLD
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8186, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.WEIGHTS_THRESHOLD' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.NOISE_NSIGMA:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F20.12)', IOSTAT=IER ) &
     &               PIM%CONF%FRIB_NOISE_NSIGMA
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8187, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.NOISE_NSIGMA' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.SNR_DETECTION:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F20.12)', IOSTAT=IER ) &
     &               PIM%CONF%FRIB_SNR_DETECTION
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8188, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.SNR_DETECTION' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.FRQ_TRANSFER_BAND:' ) THEN
              CALL CLRCH  ( STR )
              CALL TRAN ( 11, BUF(J3)(IND(1,2):IND(2,2)), STR )
              IF ( STR(1:2) == 'NO' ) THEN
                   CALL CLRCH ( PIM%CONF%FRIB_FRQ_TRANSFER_BAND )
                 ELSE
                   PIM%CONF%FRIB_FRQ_TRANSFER_BAND = BUF(J3)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=PIM%CONF%FRIB_FRQ_TRANSFER_BAND, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 8189, IUER, 'PIMA_CONF', 'Cannot '// &
     &                      'find input PIM%CONF%FRIB.FRQ_TRANSFER_BAND file '// &
     &                      PIM%CONF%FRIB_FRQ_TRANSFER_BAND(1:I_LEN(PIM%CONF%FRIB_FRQ_TRANSFER_BAND))// &
     &                      ' specified in the control file '//CONF_FILE )
                        RETURN
                   END IF
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.FRQ_TRANSFER_METHOD:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__LEGENDRE ) THEN
                   PIM%CONF%FRIB_FRQ_TRANSFER_METHOD = PIMA__LEGENDRE
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__ASIS ) THEN
                   PIM%CONF%FRIB_FRQ_TRANSFER_METHOD = PIMA__ASIS
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__SPLINE ) THEN
                   PIM%CONF%FRIB_FRQ_TRANSFER_METHOD = PIMA__SPLINE 
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+1) == PIMA__FRQTRA_NO(1:2) ) THEN
                   PIM%CONF%FRIB_FRQ_TRANSFER_METHOD = PIMA__FRQTRA_NO
                 ELSE
                   CALL ERR_LOG ( 8190, IUER, 'PIMA_CONF', 'Error during '//&
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- of FRIB.FRQ_TRANSFER_METHOD. Supported methods are '// &
     &                 PIMA__LEGENDRE//', '//PIMA__ASIS//', and '// &
     &                 PIMA__SPLINE )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.FRQ_TRANSFER_MSEG:' ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), IOSTAT=IER, FMT=* ) &
     &               PIM%CONF%FRIB_FRQ_TRANSFER_MSEG
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8191, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- error in recoding value '// &
     &                 BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.FRQ_TRANSFER_MSEG' )
                   RETURN
              END IF
              IF ( PIM%CONF%FRIB_FRQ_TRANSFER_MSEG < 1 ) THEN
                   CALL ERR_LOG ( 8192, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- wrong value '// &
     &                 BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.FRQ_TRANSFER_MSEG. '// &
     &                 'It should be greater than zero' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.FRQ_TRANSFER_DEG:' ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), IOSTAT=IER, FMT=* ) &
     &               PIM%CONF%FRIB_FRQ_TRANSFER_DEG
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8193, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- error in recoding value '// &
     &                 BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.FRQ_TRANSFER_DEG' )
                   RETURN
              END IF
              IF ( PIM%CONF%FRIB_FRQ_TRANSFER_DEG < 0 ) THEN
                   CALL ERR_LOG ( 8194, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- wrong value '// &
     &                 BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.FRQ_TRANSFER_DEG. '// &
     &                 'It should be no less than zero' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.2D_FRINGE_PLOT:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_NO ) THEN
                   PIM%CONF%FRIB_2D_FRINGE_PLOT =  PIMA__PLOT_NO
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_XW ) THEN
                   PIM%CONF%FRIB_2D_FRINGE_PLOT =  PIMA__PLOT_XW
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_PS ) THEN
                   PIM%CONF%FRIB_2D_FRINGE_PLOT =  PIMA__PLOT_PS
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_GIF ) THEN
                   PIM%CONF%FRIB_2D_FRINGE_PLOT =  PIMA__PLOT_GIF
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_TXT ) THEN
                   PIM%CONF%FRIB_2D_FRINGE_PLOT =  PIMA__PLOT_TXT
                 ELSE
                   CALL ERR_LOG ( 8195, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.2D_FRINGE_PLOT' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.1D_RESTIM_PLOT:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_NO ) THEN
                   PIM%CONF%FRIB_1D_RESTIM_PLOT  =  PIMA__PLOT_NO
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_XW ) THEN
                   PIM%CONF%FRIB_1D_RESTIM_PLOT  =  PIMA__PLOT_XW
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_PS ) THEN
                   PIM%CONF%FRIB_1D_RESTIM_PLOT  =  PIMA__PLOT_PS
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_GIF ) THEN
                   PIM%CONF%FRIB_1D_RESTIM_PLOT  =  PIMA__PLOT_GIF
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+2) == PIMA__PLOT_TXT ) THEN
                   PIM%CONF%FRIB_1D_RESTIM_PLOT  =  PIMA__PLOT_TXT
!@                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_SAV ) THEN
!@                   PIM%CONF%FRIB_1D_RESTIM_PLOT  =  PIMA__PLOT_SAV
                 ELSE
                   CALL ERR_LOG ( 8196, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.1D_RESTIM_PLOT' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.1D_RESFRQ_PLOT:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_NO ) THEN
                   PIM%CONF%FRIB_1D_RESFRQ_PLOT  =  PIMA__PLOT_NO
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_XW ) THEN
                   PIM%CONF%FRIB_1D_RESFRQ_PLOT  =  PIMA__PLOT_XW
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_PS ) THEN
                   PIM%CONF%FRIB_1D_RESFRQ_PLOT  =  PIMA__PLOT_PS
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_GIF ) THEN
                   PIM%CONF%FRIB_1D_RESFRQ_PLOT  =  PIMA__PLOT_GIF
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+2) == PIMA__PLOT_TXT ) THEN
                   PIM%CONF%FRIB_1D_RESFRQ_PLOT  =  PIMA__PLOT_TXT
!@                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_SAV ) THEN
!@                   PIM%CONF%FRIB_1D_RESFRQ_PLOT  =  PIMA__PLOT_SAV
                 ELSE
                   CALL ERR_LOG ( 8197, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.1D_RESFRQ_PLOT' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.1D_DRF_PLOT:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_NO ) THEN
                   PIM%CONF%FRIB_1D_DRF_PLOT  =  PIMA__PLOT_NO
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_XW ) THEN
                   PIM%CONF%FRIB_1D_DRF_PLOT  =  PIMA__PLOT_XW
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_PS ) THEN
                   PIM%CONF%FRIB_1D_DRF_PLOT  =  PIMA__PLOT_PS
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__PLOT_GIF ) THEN
                   PIM%CONF%FRIB_1D_DRF_PLOT  =  PIMA__PLOT_GIF
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+2) == PIMA__PLOT_TXT ) THEN
                   PIM%CONF%FRIB_1D_DRF_PLOT  =  PIMA__PLOT_TXT
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+2) == PIMA__PLOT_SAV ) THEN
                   PIM%CONF%FRIB_1D_DRF_PLOT  =  PIMA__PLOT_SAV
                 ELSE
                   CALL ERR_LOG ( 8198, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.1D_DRF_PLOT' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.OVERSAMPLE_PLOT_MD:' ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I4)', IOSTAT=IER ) &
     &               PIM%CONF%FRIB_OVERSAMPLE_PLOT_MD
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8199, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.OVERSAMPLE_PLOT_MD' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.OVERSAMPLE_PLOT_RT:' ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I4)', IOSTAT=IER ) &
     &               PIM%CONF%FRIB_OVERSAMPLE_PLOT_RT
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8200, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.OVERSAMPLE_PLOT_RT' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.PLOT_DELAY_WINDOW_WIDTH:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F20.12)', IOSTAT=IER ) &
     &               PIM%CONF%FRIB_PLOT_DELAY_WINDOW_WIDTH
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8201, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.PLOT_DELAY_WINDOW_WIDTH' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'FRIB.PLOT_RATE_WINDOW_WIDTH:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F20.12)', IOSTAT=IER ) &
     &               PIM%CONF%FRIB_PLOT_RATE_WINDOW_WIDTH
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8202, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- failure to decode value '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.PLOT_RATE_WINDOW_WIDTH' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'FRIB.1D_TIM_MSEG:' ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), IOSTAT=IER, FMT=* ) &
     &               PIM%CONF%FRIB_1D_TIM_MSEG
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8203, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- error in recoding value '// &
     &                 BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.1D_TIM_MSEG' )
                   RETURN
              END IF
              IF ( PIM%CONF%FRIB_1D_TIM_MSEG < 1 ) THEN
                   CALL ERR_LOG ( 8204, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- wrong value '// &
     &                 BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.1D_TIM_MSEG. '// &
     &                 'It should be greater than zero' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'FRIB.1D_FRQ_MSEG:' ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), IOSTAT=IER, FMT=* ) &
     &               PIM%CONF%FRIB_1D_FRQ_MSEG
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8205, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- error in recoding value '// &
     &                 BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.1D_FRQ_MSEG' )
                   RETURN
              END IF
              IF ( PIM%CONF%FRIB_1D_FRQ_MSEG < 1 ) THEN
                   CALL ERR_LOG ( 8206, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- wrong value '// &
     &                 BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.1D_FRQ_MSEG. '// &
     &                 'It should be greater than zero' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'FRIB.1D_DRF_SPAN:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), IOSTAT=IER, &
     &               FMT='(F20.0)' ) PIM%CONF%FRIB_1D_DRF_SPAN
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8207, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- error in recoding value '// &
     &                 BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.1D_DRF_SPAN' )
                   RETURN
              END IF
              IF ( PIM%CONF%FRIB_1D_DRF_SPAN < 0.001 ) THEN
                   CALL ERR_LOG ( 8208, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- wrong value '// &
     &                 BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword FRIB.1D_DRF_SPAN. '// &
     &                 'It should be greater than 0.001' )
                   RETURN
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'OBS:' .OR. &
     &                BUF(J3)(IND(1,1):IND(2,1)) == 'FRIB.OBS:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__OBS_NO ) THEN
                   PIM%CONF%FRIB_OBS_COMMAND = PIMA__OBS_NO
                   PIM%CONF%FRIB_OBS_STATUS  = PIMA__OBS_NO
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__OBS_ALL ) THEN
                   PIM%CONF%FRIB_OBS_COMMAND = PIMA__OBS_ALL
                   PIM%CONF%FRIB_OBS_STATUS  = PIMA__OBS_ALL
                   PIM%CONF%FRIB_NOBS        = -1
                   PIM%CONF%FRIB_FIRST_OBS   = -1
                   PIM%CONF%FRIB_LAST_OBS    = -1
                 ELSE
                   IVAL  = 0
                   IVAL1 = 0
                   IVAL2 = 0
                   PIM%CONF%FRIB_OBS_COMMAND = BUF(J3)(IND(1,2):IND(2,2))
                   IP = INDEX ( PIM%CONF%FRIB_OBS_COMMAND, ':' )
                   IF ( IP == 1 ) THEN
                        CALL ERR_LOG ( 8209, IUER, 'PIMA_CONF', &
     &                      'Wrong value of the keyword OBS: '// &
     &                       PIM%CONF%FRIB_OBS_COMMAND(1:I_LEN(PIM%CONF%FRIB_OBS_COMMAND))// &
     &                       ' -- an integer value should precede '// &
     &                       'a colon sign' )
                        RETURN
                      ELSE IF ( IP == ILEN(BUF(J3)(IND(1,2):IND(2,2))) ) THEN
                        CALL ERR_LOG ( 8210, IUER, 'PIMA_CONF', &
     &                      'Wrong value of the keyword OBS: '// &
     &                       PIM%CONF%FRIB_OBS_COMMAND(1:I_LEN(PIM%CONF%FRIB_OBS_COMMAND))// &
     &                       ' -- an integer value should follow a colon sign' )
                        RETURN
                      ELSE IF ( IP > 0 ) THEN
                        READ ( UNIT=PIM%CONF%FRIB_OBS_COMMAND(1:IP-1), &
     &                         FMT='(I8)', IOSTAT=IER ) IVAL1
                        IF ( IER .NE. 0 .OR. IVAL1 .LE. 0 ) THEN
                             CALL ERR_LOG ( 8211, IUER, 'PIMA_CONF', &
     &                           'Wrong value of the keyword OBS: '// &
     &                            PIM%CONF%FRIB_OBS_COMMAND(1:IP)// &
     &                            ' -- an integer > 0 before the colon sign '// &
     &                            'was expected' )
                             RETURN
                        END IF
!
                        READ ( UNIT=PIM%CONF%FRIB_OBS_COMMAND(IP+1:), &
     &                         FMT='(I8)', IOSTAT=IER ) IVAL2
                        IF ( IER .NE. 0 .OR. IVAL2 .LT. -1  ) THEN
                             CALL ERR_LOG ( 8212, IUER, 'PIMA_CONF', &
     &                           'Wrong value of the keyword OBS: '// &
     &                            PIM%CONF%FRIB_OBS_COMMAND(IP+1:ILEN(PIM%CONF%FRIB_OBS_COMMAND))// &
     &                            ' -- an integer > 0 after the colon sign '// &
     &                            'was expected' )
                             RETURN
                        END IF
                        IF ( IVAL2 == 0 .OR. IVAL2 == -1 ) THEN
                             FIL_STT = TRIM(PIM%CONF%EXPER_DIR)//'/'//TRIM(PIM%CONF%SESS_CODE)//'.stt'
                             INQUIRE ( FILE=FIL_STT, EXIST=LEX )
                             IF ( .NOT. LEX ) THEN
                                  CALL ERR_LOG ( 8213, IUER, 'PIMA_CONF', &
     &                                'Wrong value of the keyword OBS: '// &
     &                                 PIM%CONF%FRIB_OBS_COMMAND(1:ILEN(PIM%CONF%FRIB_OBS_COMMAND))// &
     &                                ' -- 0 or -1 can be used after the experiment is loaded' )
                                  RETURN
                             END IF
                             CALL ERR_PASS ( IUER, IER )
                             CALL RD_TEXT ( FIL_STT, MBUF, BUFS, NS, IER )
                             IF ( IER .NE. 0 ) THEN
                                  CALL ERR_LOG ( 8214, IUER, 'PIMA_CONF', 'Failure in reading '// &
     &                                'auxilliary file '//TRIM(FIL_STT)//' Please reload '// &
     &                                'the experiment' )
                                  RETURN
                             END IF
                             PIM%NOBS = 0
                             DO 540 J4=1,NS
                                IF ( BUFS(J4)(1:23) == 'Number of observations:' ) THEN
                                     STR = BUFS(J4)(24:)
                                     CALL CHASHL ( STR )
                                     CALL CHIN ( STR, PIM%NOBS )
                                END IF
 540                         CONTINUE 
                             IF ( PIM%NOBS == 0 ) THEN
                                  CALL ERR_LOG ( 8215, IUER, 'PIMA_CONF', 'Trap of internal '// &
     &                                'control: the number of observations has not been '// &
     &                                'found in the auxilliary file '//FIL_STT )
                                  RETURN
                             END IF
                             IVAL2 = PIM%NOBS
                          ELSE IF ( IVAL2 < IVAL1 ) THEN
                             CALL ERR_LOG ( 8216, IUER, 'PIMA_CONF', &
     &                           'Wrong value of the keyword OBS: '// &
     &                            PIM%CONF%FRIB_OBS_COMMAND(1:ILEN(PIM%CONF%FRIB_OBS_COMMAND))// &
     &                            ' -- the value after the column sign '// &
     &                            'should be equal or greater than the '// &
     &                            'value beftore the column sign' )
                             RETURN
                        END IF
                      ELSE
                        READ ( UNIT=PIM%CONF%FRIB_OBS_COMMAND, FMT='(I8)', &
     &                        IOSTAT=IER ) IVAL
                        IF ( IER == 0 .AND. IVAL .LE. 0 ) THEN
                             CALL ERR_LOG ( 8217, IUER, 'PIMA_CONF', &
     &                           'Wrong value of the keyword OBS: '// &
     &                            PIM%CONF%FRIB_OBS_COMMAND(1:IP), &
     &                            ' -- an integer > 0 value was expected' )
                             RETURN
                           ELSE IF ( IER .NE. 0 ) THEN
                             IVAL = -1
                        END IF
                   END IF
!
                   IF ( IVAL > 0 ) THEN
!
! --------------------- The specific observation index is specified
!
                        PIM%CONF%FRIB_OBS_STATUS = PIMA__OBS_ONE
                        PIM%CONF%FRIB_NOBS = 1
                        PIM%CONF%FRIB_FIRST_OBS = IVAL
                        PIM%CONF%FRIB_LAST_OBS  = IVAL
                        ALLOCATE ( PIM%CONF%FRIB_OBS(PIM%CONF%FRIB_NOBS), &
     &                             STAT=IER )
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH ( STR )
                             CALL IINCH ( 4*PIM%CONF%FRIB_NOBS, STR )
                             CALL ERR_LOG ( 8218, IUER, 'PIMA_CONF', &
     &                           'Failure to allocate '//STR(1:I_LEN(STR))// &
     &                           'bytes of dynamic memory for the list '// &
     &                           'of observations' )
                             RETURN
                        END IF
                        PIM%CONF%FRIB_OBS(1) = IVAL
                      ELSE IF ( IVAL ==  0 ) THEN
!
! --------------------- The range of observation indexes is specified
!
                        PIM%CONF%FRIB_NOBS = IVAL2 - IVAL1 + 1
                        ALLOCATE ( PIM%CONF%FRIB_OBS(PIM%CONF%FRIB_NOBS), &
     &                             STAT=IER )
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH ( STR )
                             CALL IINCH ( 4*PIM%CONF%FRIB_NOBS, STR )
                             CALL ERR_LOG ( 8219, IUER, 'PIMA_CONF', &
     &                           'Failure to allocate '//STR(1:I_LEN(STR))// &
     &                           'bytes of dynamic memory for the list '// &
     &                           'of observations' )
                             RETURN
                        END IF
!
                        IP = 0
                        DO 450 J5=IVAL1, IVAL2
                           IP = IP + 1
                           PIM%CONF%FRIB_OBS(IP) = J5
 450                    CONTINUE
                      ELSE IF ( IVAL == -1 ) THEN
!
! --------------------- The file name with observations indexes is specified
!
                        PIM%CONF%FRIB_OBS_STATUS = PIMA__OBS_ONE
                        ALLOCATE ( BUF_OBS(PIM__MEPC), STAT=IER )
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH ( STR )
                             CALL IINCH ( LEN(BUF_OBS(1))*PIM__MEPC, STR )
                             CALL ERR_LOG ( 8220, IUER, 'PIMA_CONF', &
     &                           'Failure to allocate '//STR(1:I_LEN(STR))// &
     &                           'bytes of dynamic memory for the buffer '// &
     &                           'of the file with the list of observations' )
                             RETURN
                        END IF
!
                        CALL ERR_PASS ( IUER, IER )
                        CALL RD_TEXT ( PIM%CONF%FRIB_OBS_COMMAND, PIM__MEPC, &
     &                                 BUF_OBS, NB, IER )
                        IF ( IER .NE. 0 ) THEN
                             CALL ERR_LOG ( 8221, IUER, 'PIMA_CONF', &
     &                           'Failure to read the file with '// &
     &                           'observations list '// &
     &                            PIM%CONF%FRIB_OBS_COMMAND )
                             DEALLOCATE ( BUF_OBS )
                             RETURN
                        END IF
!
                        PIM%CONF%FRIB_NOBS = 0
                        DO 460 J6=1,NB
                           IF ( BUF_OBS(J6)(1:1)  == '#' ) GOTO 460
                           IF ( ILEN(BUF_OBS(J6)) ==  0  ) GOTO 460
                           PIM%CONF%FRIB_NOBS = PIM%CONF%FRIB_NOBS + 1
 460                    CONTINUE
!
                        ALLOCATE ( PIM%CONF%FRIB_OBS(PIM%CONF%FRIB_NOBS), &
     &                             STAT=IER )
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH ( STR )
                             CALL IINCH ( 4*PIM%CONF%FRIB_NOBS, STR )
                             CALL ERR_LOG ( 8222, IUER, 'PIMA_CONF', &
     &                           'Failure to allocate '//STR(1:I_LEN(STR))// &
     &                           'bytes of dynamic memory for the list '// &
     &                           'of observations' )
                             DEALLOCATE ( BUF_OBS )
                             RETURN
                        END IF
!
                        IP = 0
                        DO 470 J7=1,NB
                           IF ( BUF_OBS(J7)(1:1)  == '#' ) GOTO 470
                           IF ( ILEN(BUF_OBS(J7)) ==  0  ) GOTO 470
                           IP = IP + 1
                           CALL CHASHL ( BUF_OBS(J7) )
                           IL = INDEX ( BUF_OBS(J7), ' ' )
                           IF ( IL .LE. 0 ) IL = ILEN(BUF_OBS(J7)) + 1
                           READ ( UNIT=BUF_OBS(J7)(1:IL-1), FMT='(I8)', &
     &                            IOSTAT=IER ) IVAL
                           IF ( IER .NE. 0 ) THEN
                                CALL CLRCH ( STR )
                                CALL INCH  ( J7, STR )
                                CALL ERR_LOG ( 8223, IUER, 'PIMA_CONF', &
     &                              'Failure to decode the '// &
     &                               STR(1:I_LEN(STR))//'th line of the '// &
     &                              'observation file '// &
     &                               PIM%CONF%FRIB_OBS_COMMAND(1:I_LEN(PIM%CONF%FRIB_OBS_COMMAND))// &
     &                              ' -- '// &
     &                               BUF_OBS(J7)(1:I_LEN(BUF_OBS(J7))) )
                                DEALLOCATE ( BUF_OBS )
                                RETURN
                           END IF
                           PIM%CONF%FRIB_OBS(IP) = IVAL
 470                    CONTINUE
                        DEALLOCATE ( BUF_OBS )
                   END IF
              END IF
              LFRIB_KEY = LFRIB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.STA_INC_FILE:' ) THEN
              PIM%CONF%FRIP_STA_INC_FILE = BUF(J3)(IND(1,2):IND(2,2))
              IF ( PIM%CONF%FRIP_STA_INC_FILE .EQ. 'NO' ) THEN
                   CONTINUE
                 ELSE
                   INQUIRE ( FILE=PIM%CONF%FRIP_STA_INC_FILE, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 8224, IUER, 'PIMA_CONF', 'Cannot '// &
     &                      'find include scan include file '// &
     &                       PIM%CONF%FRIP_STA_INC_FILE(1:I_LEN(PIM%CONF%FRIP_STA_INC_FILE))// &
     &                      ' specified in the control file '//CONF_FILE )
                        RETURN
                   END IF
              END IF
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.STA_EXC_FILE:' ) THEN
              PIM%CONF%FRIP_STA_EXC_FILE = BUF(J3)(IND(1,2):IND(2,2))
              IF ( PIM%CONF%FRIP_STA_EXC_FILE .EQ. 'NO' ) THEN
                   CONTINUE
                 ELSE
                   INQUIRE ( FILE=PIM%CONF%FRIP_STA_EXC_FILE, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 8225, IUER, 'PIMA_CONF', 'Cannot '// &
     &                      'find include scan exclude file '// &
     &                       PIM%CONF%FRIP_STA_EXC_FILE(1:I_LEN(PIM%CONF%FRIP_STA_EXC_FILE))// &
     &                      ' specified in the control file '//CONF_FILE )
                        RETURN
                   END IF
              END IF
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.STA_REFS:' ) THEN
              PIM%CONF%FRIP_N_STA_REF = 0
              DO 480 J8=2,LIND
                 PIM%CONF%FRIP_STA_REFS(J8-1) = BUF(J3)(IND(1,J8):IND(2,J8))
                 IF ( PIM%CONF%FRIP_STA_REFS(J8-1) == 'SAME' ) THEN
                      PIM%CONF%FRIP_STA_REFS(J8-1) = PIM%CONF%STA_REF
                 END IF
                 PIM%CONF%FRIP_N_STA_REF = PIM%CONF%FRIP_N_STA_REF + 1
 480          CONTINUE
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.RESOLUTION:' ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I6)', IOSTAT=IER ) PIM%CONF%FRIP_RESOLUTION
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8226, IUER, 'PIMA_CONF', 'Wrong value '// &
     &                 'of the keyword FRIP.RESOLUTION: '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- an integer value was expected' )
                   RETURN
              END IF
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.SCA:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__SCA_NO ) THEN
                   PIM%CONF%FRIP_SCA_STATUS  = PIMA__SCA_NO
                   PIM%CONF%FRIP_FIRST_SCA  = 0
                   PIM%CONF%FRIP_LAST_SCA   = 0
                 ELSE
                   IVALS = 0
                   IVAL1 = 0
                   IVAL2 = 0
                   SCA_COMMAND = BUF(J3)(IND(1,2):IND(2,2))
                   IP = INDEX ( SCA_COMMAND, ':' )
                   IF ( IP == 1 ) THEN
                        CALL ERR_LOG ( 8227, IUER, 'PIMA_CONF', &
     &                      'Wrong value of the keyword FRIP.SCA: '// &
     &                       SCA_COMMAND(1:I_LEN(SCA_COMMAND))// &
     &                       ' -- an integer value should precede '// &
     &                       'a colon sign' )
                        RETURN
                      ELSE IF ( IP == ILEN(BUF(J3)(IND(1,2):IND(2,2))) ) THEN
                        CALL ERR_LOG ( 8228, IUER, 'PIMA_CONF', &
     &                      'Wrong value of the keyword FRIP.SCA: '// &
     &                       SCA_COMMAND(1:I_LEN(SCA_COMMAND))// &
     &                       ' -- an integer value should follow a colon sign' )
                        RETURN
                      ELSE IF ( IP > 0 ) THEN
                        READ ( UNIT=SCA_COMMAND(1:IP-1), &
     &                         FMT='(I8)', IOSTAT=IER ) IVAL1
                        IF ( IER .NE. 0 .OR. IVAL1 .LE. 0 ) THEN
                             CALL ERR_LOG ( 8229, IUER, 'PIMA_CONF', &
     &                           'Wrong value of the keyword FRIP.SCA: '// &
     &                            SCA_COMMAND(1:IP)// &
     &                            ' -- an integer > 0 before the colon sign '// &
     &                            'was expected' )
                             RETURN
                        END IF
!
                        READ ( UNIT=SCA_COMMAND(IP+1:), &
     &                         FMT='(I8)', IOSTAT=IER ) IVAL2
                        IF ( IER .NE. 0 .OR. IVAL2 .LE. 0  ) THEN
                             CALL ERR_LOG ( 8230, IUER, 'PIMA_CONF', &
     &                           'Wrong value of the keyword FRIP.SCA: '// &
     &                            SCA_COMMAND(IP+1:ILEN(SCA_COMMAND))// &
     &                            ' -- an integer > 0 after the colon sign '// &
     &                            'was expected' )
                             RETURN
                        END IF
                        IF ( IVAL2 < IVAL1 ) THEN
                             CALL ERR_LOG ( 8231, IUER, 'PIMA_CONF', &
     &                           'Wrong value of the keyword FRIP.SCA: '// &
     &                            SCA_COMMAND(1:ILEN(SCA_COMMAND))// &
     &                            ' -- the value after the column sign '// &
     &                            'should be equal or greater than the '// &
     &                            'value beftore the column sign' )
                             RETURN
                        END IF
                        PIM%CONF%FRIP_SCA_STATUS = PIMA__SCA_LIST
                      ELSE IF ( SCA_COMMAND == PIMA__SCA_ALL ) THEN
                        IVAL1 = 1
                        IVAL2 = PIM%NSCA
                        PIM%CONF%FRIP_SCA_STATUS = PIMA__SCA_ALL
                      ELSE
                        READ ( UNIT=SCA_COMMAND, FMT='(I8)', &
     &                        IOSTAT=IER ) IVAL1
                        IF ( IER == 0 .AND. IVAL1 .LE. 0 ) THEN
                             CALL ERR_LOG ( 8232, IUER, 'PIMA_CONF', &
     &                           'Wrong value of the keyword FRIP.SCA: '// &
     &                            SCA_COMMAND(1:IP), &
     &                            ' -- an integer > 0 value was expected' )
                             RETURN
                           ELSE IF ( IER .NE. 0 ) THEN
                             PIM%CONF%FRIP_SOU = SCA_COMMAND
                             IVAL1 = -1
                        END IF
                        IVAL2 = IVAL1
                        PIM%CONF%FRIP_SCA_STATUS = PIMA__SCA_ONE
                   END IF
                   PIM%CONF%FRIP_FIRST_SCA = IVAL1
                   PIM%CONF%FRIP_LAST_SCA  = IVAL2
              END IF
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.SCAN_FILE:' ) THEN
              PIM%CONF%FRIP_SCAN_FILE = BUF(J3)(IND(1,2):IND(2,2))
              IF ( PIM%CONF%FRIP_SCAN_FILE .EQ. 'NO' ) THEN
                   CONTINUE
                 ELSE
                   INQUIRE ( FILE=PIM%CONF%FRIP_SCAN_FILE, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 8233, IUER, 'PIMA_CONF', 'Cannot '// &
     &                      'find include scan file '// &
     &                       PIM%CONF%FRIP_SCAN_FILE(1:I_LEN(PIM%CONF%FRIP_SCAN_FILE))// &
     &                      ' specified in the control file '//CONF_FILE )
                        RETURN
                   END IF
!
                   ALLOCATE ( BUF_SCA(PIM__MSCA+128), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( LEN(BUF_SCA(1))*(PIM__MSCA+128), STR )
                        CALL ERR_LOG ( 8234, IUER, 'PIMA_CONF', &
     &                      'Failure to allocate '//STR(1:I_LEN(STR))// &
     &                      'bytes of dynamic memory for the list '// &
     &                      'of scans with phase referencing source' )
                        RETURN
                   END IF
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL RD_TEXT ( PIM%CONF%FRIP_SCAN_FILE, PIM__MSCA+128, &
     &                            BUF_SCA, NB, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8235, IUER, 'PIMA_CONF', &
     &                      'Failure to read the file with '// &
     &                      'observations list '// &
     &                       PIM%CONF%FRIP_SCAN_FILE )
                        DEALLOCATE ( BUF_SCA )
                        RETURN
                   END IF
!
                   PIM%CONF%FRIP_NSCA = 0
                   DO 490 J9=1,NB
                      IF ( BUF_SCA(J9)(1:1)  == '#' ) GOTO 490
                      IF ( ILEN(BUF_SCA(J9)) ==  0  ) GOTO 490
                      PIM%CONF%FRIP_NSCA = PIM%CONF%FRIP_NSCA + 1
 490               CONTINUE
!
                   ALLOCATE ( PIM%CONF%FRIP_SCA(0:PIM__MPCS,PIM%CONF%FRIP_NSCA), &
     &                        STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( 4*PIM%CONF%FRIP_NSCA, STR )
                        CALL ERR_LOG ( 8236, IUER, 'PIMA_CONF', &
     &                      'Failure to allocate '//STR(1:I_LEN(STR))// &
     &                      'bytes of dynamic memory for the list '// &
     &                      'of observations' )
                        DEALLOCATE ( BUF_SCA )
                        RETURN
                   END IF
!
                   IP = 0
                   PIM%CONF%FRIP_FIRST_SCA  = PIM__MSCA+1
                   PIM%CONF%FRIP_LAST_SCA   = 0
                   DO 4100 J10=1,NB
                      IF ( BUF_SCA(J10)(1:1)  == '#' ) GOTO 4100
                      IF ( ILEN(BUF_SCA(J10)) ==  0  ) GOTO 4100
                      IP = IP + 1
                      CALL EXWORD ( BUF_SCA(J10), MIND, LIND_SCA, &
     &                              IND_SCA, REG, IER )
!!                      DO 4110 J11=1,MIN(LIND_SCA,PIM__MPCS)
                      DO 4110 J11=1,2
                         READ ( UNIT=BUF_SCA(J10)(IND_SCA(1,J11):IND_SCA(2,J11)), &
     &                          FMT='(I8)', IOSTAT=IER ) IVAL
                         IF ( IER .NE. 0 ) THEN
                              CALL CLRCH ( STR )
                              CALL INCH  ( J10, STR )
                              CALL ERR_LOG ( 8237, IUER, 'PIMA_CONF', &
     &                            'Failure to decode the '// &
     &                             STR(1:I_LEN(STR))//'th line of the '// &
     &                            'observation file '// &
     &                             PIM%CONF%FRIP_SCAN_FILE(1:I_LEN(PIM%CONF%FRIP_SCAN_FILE))// &
     &                            ' -- '// &
     &                             BUF_SCA(J10)(1:I_LEN(BUF_SCA(J10))) )
                              DEALLOCATE ( BUF_SCA )
                              RETURN
                          END IF
                          PIM%CONF%FRIP_SCA(J11-1,IP) = IVAL
 4110                 CONTINUE
                      PIM%CONF%FRIP_FIRST_SCA = MIN(PIM%CONF%FRIP_SCA(0,IP), &
     &                                              PIM%CONF%FRIP_FIRST_SCA)
                      PIM%CONF%FRIP_LAST_SCA  = MAX(PIM%CONF%FRIP_SCA(0,IP), &
     &                                              PIM%CONF%FRIP_FIRST_SCA)
 4100              CONTINUE
                   DEALLOCATE ( BUF_SCA )
!
                   IF ( IVALS > 0 ) THEN
!
! --------------------- The specific observation index is specified
!
                        PIM%CONF%FRIP_SCA_STATUS = PIMA__SCA_ONE
                        PIM%CONF%FRIP_FIRST_SCA  = IVALS
                        PIM%CONF%FRIP_LAST_SCA   = IVALS
                     ELSE IF ( IVALS ==  0 ) THEN
!
! --------------------- The range of observation indexes is specified
!
                        PIM%CONF%FRIP_SCA_STATUS = PIMA__SCA_LIST
                        PIM%CONF%FRIP_FIRST_SCA = IVAL1
                        PIM%CONF%FRIP_LAST_SCA  = IVAL2
                     ELSE IF ( IVALS == -1 ) THEN
                        IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__SCA_ALL ) THEN
!
! -------------------------- Use all scans specified in the file
!
                             PIM%CONF%FRIP_SCA_STATUS = PIMA__SCA_ALL
                           ELSE
                             CALL ERR_LOG ( 8238, IUER, 'PIMA_CONF', &
     &                           'Wrong value of the keyword FRIP.SCA: '// &
     &                            BUF(J3)(IND(1,2):IND(2,2))//' a scan '// &
     &                           'index, or a scan index range, or ALL, '// &
     &                           'or NO were expected' )
                             RETURN
                        END IF
                   END IF
              END IF
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.OVERSAMPLE:' ) THEN
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.MAP_DIR:' ) THEN
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.ATM_ZEN_FILE:' ) THEN
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.CAL_PLOT:' ) THEN
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.CAL_RES:' ) THEN
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.TAG_PLOT:' ) THEN
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.TAG_RES:' ) THEN
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.BEAM_PLOT:' ) THEN
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.FRQ_MSEG:' ) THEN
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.TIM_MSEG:' ) THEN
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.RA_CENTER:' ) THEN
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.DEC_CENTER:' ) THEN
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.RA_STEP:' ) THEN
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.DEC_STEP:' ) THEN
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.RA_RANGE:' ) THEN
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &               'FRIP.DEC_RANGE:' ) THEN
              LFRIP_KEY = LFRIP_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'MKDB.OUTPUT_TYPE:' ) THEN
              CALL TRAN ( 11, BUF(J3)(IND(1,2):IND(2,2)), BUF(J3)(IND(1,2):IND(2,2)) )
              IF  ( BUF(J3)(IND(1,2):IND(2,2)) == 'TEXT' ) THEN
                    PIM%CONF%MKDB_OUTPUT_TYPE = PIMA__MKDB_TEXT
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'GVF' ) THEN
                    PIM%CONF%MKDB_OUTPUT_TYPE = PIMA__MKDB_GVF
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'AMPL' ) THEN
                    PIM%CONF%MKDB_OUTPUT_TYPE = PIMA__MKDB_AMPL
                 ELSE
                    CALL ERR_LOG ( 8239, IUER, 'PIMA_CONF', 'Wrong '// &
     &                  'value of MKDB.OUTPUT_TYPE: '// &
     &                   BUF(J3)(IND(1,2):IND(2,2))//' one of TEXT, AMPL '// &
     &                  'or GVF were expected' )
                    RETURN
              END IF
              LMKDB_KEY = LMKDB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'MKDB.SRT:' ) THEN
              CALL CLRCH ( STR )
              CALL TRAN ( 11, BUF(J3)(IND(1,2):IND(2,2)), STR )
              IF ( STR(1:8) == PIMA__MKDB_MID_SCAN ) THEN
                   PIM%CONF%MKDB_SRT_TYPE = PIMA__MKDB_MID_SCAN
                   CALL CLRCH ( PIM%CONF%MKDB_SRT_FILE )
                 ELSE IF ( STR(1:8) == PIMA__MKDB_SRT_FRT ) THEN
                   PIM%CONF%MKDB_SRT_TYPE = PIMA__MKDB_SRT_FRT
                   CALL CLRCH ( PIM%CONF%MKDB_SRT_FILE )
                 ELSE
                   PIM%CONF%MKDB_SRT_TYPE = PIMA__MKDB_FILE
                   PIM%CONF%MKDB_SRT_FILE = BUF(J3)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=PIM%CONF%MKDB_SRT_FILE, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 8240, IUER, 'PIMA_CONF', 'Cannot '// &
     &                      'find input MKDB.SRT file '// &
     &                      PIM%CONF%MKDB_SRT_FILE(1:I_LEN(PIM%CONF%MKDB_SRT_FILE))// &
     &                      ' specified in the control file '//CONF_FILE )
                        RETURN
                   END IF
              END IF
              LMKDB_KEY = LMKDB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'MKDB.2ND_BAND:' ) THEN
              CALL CLRCH  ( STR )
              CALL TRAN ( 11, BUF(J3)(IND(1,2):IND(2,2)), STR )
              IF ( STR == 'NO' ) THEN
                   CALL CLRCH ( PIM%CONF%MKDB_2ND_BAND_FILE )
                 ELSE
                   PIM%CONF%MKDB_2ND_BAND_FILE = BUF(J3)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=PIM%CONF%MKDB_2ND_BAND_FILE, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 8241, IUER, 'PIMA_CONF', 'Cannot '// &
     &                      'find input MKDB.2ND_BAND_FILE file '// &
     &                      PIM%CONF%MKDB_2ND_BAND_FILE(1:I_LEN(PIM%CONF%MKDB_2ND_BAND_FILE))// &
     &                      ' specified in the control file '//CONF_FILE )
                        RETURN
                   END IF
              END IF
              LMKDB_KEY = LMKDB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == &
     &                'MKDB.FRINGE_ALGORITHM:' ) THEN
              PIM%CONF%MKDB_FRINGE_ALGORITHM = BUF(J3)(IND(1,2):IND(2,2))
              IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__FRA_DRF ) THEN
                   CONTINUE
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__FRA_LSQ ) THEN
                   CONTINUE
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__FRA_MUL ) THEN
                   CONTINUE
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__FRA_ADD ) THEN
                   CONTINUE
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__FINE_SEARCH_NO ) THEN
                   CONTINUE
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__FINE_SEARCH_PAR ) THEN
                   PIM%CONF%MKDB_FRINGE_ALGORITHM = PIMA__FRA_DRF 
                 ELSE
                   CALL ERR_LOG ( 8242, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unsupported value of '// &
     &                  BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword MKDB.FRINGE_ALGORITHM' )
                   RETURN
              END IF
              LMKDB_KEY = LMKDB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'MKDB.FILTER:' ) THEN
              CALL CLRCH ( STR )
              CALL TRAN ( 11, BUF(J3)(IND(1,2):IND(2,2)), STR )
              IF ( STR == 'NO' ) THEN
                   PIM%CONF%MKDB_FILTER = PIMA__FILTER_NO
                 ELSE IF ( STR == 'ONLY_DET' ) THEN
                   PIM%CONF%MKDB_FILTER = PIMA__ONLY_DET
                 ELSE
                   CALL ERR_LOG ( 8243, IUER, 'PIMA_CONF', 'Cannot '// &
     &                 'Unsupported value of the keyword PIM%CONF%MKDB_FILTER: '// &
     &                      PIM%CONF%MKDB_FILTER(1:I_LEN(PIM%CONF%MKDB_FILTER))// &
     &                      ' as specified in the control file '// &
     &                      CONF_FILE(1:I_LEN(CONF_FILE))//' -- only '// &
     &                      'NO or ONLY_DET are supported' )
                   RETURN
              END IF
              LMKDB_KEY = LMKDB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'MKDB.GD_MAX_ADD_ERROR:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), IOSTAT=IER, &
     &               FMT='(F20.0)' ) PIM%CONF%MKDB_GD_MAX_ADD_ERROR
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8244, IUER, 'PIMA_CONF', 'Failure to '// &
     &                 'decode MKDB.GD_MAX_ADD_ERROR '// &
     &                 ' as specified in the control file '// &
     &                 CONF_FILE(1:I_LEN(CONF_FILE))//' -- a real value '// &
     &                 'was expected but got '// &
     &                 BUF(J3)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              LMKDB_KEY = LMKDB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'MKDB.GD_MAX_SCL_ERROR:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), IOSTAT=IER, &
     &               FMT='(F20.0)' ) PIM%CONF%MKDB_GD_MAX_SCL_ERROR
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8245, IUER, 'PIMA_CONF', 'Failure to '// &
     &                 'decode MKDB.GD_MAX_SCL_ERROR '// &
     &                 ' as specified in the control file '// &
     &                 CONF_FILE(1:I_LEN(CONF_FILE))//' -- a real value '// &
     &                 'was expected but got '// &
     &                 BUF(J3)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              LMKDB_KEY = LMKDB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'MKDB.VCAT_CONFIG:' ) THEN
              CALL CLRCH ( STR )
              CALL TRAN ( 11, BUF(J3)(IND(1,2):IND(2,2)), STR )
              IF ( STR == 'NO' ) THEN
                   CALL CLRCH ( PIM%CONF%MKDB_VCAT_CONFIG )
                 ELSE
                   PIM%CONF%MKDB_VCAT_CONFIG = BUF(J3)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=PIM%CONF%MKDB_VCAT_CONFIG, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 8246, IUER, 'PIMA_CONF', 'Cannot '// &
     &                      'find input MKDB.VCAT_CONFIG file '// &
     &                      PIM%CONF%MKDB_VCAT_CONFIG(1:I_LEN(PIM%CONF%MKDB_VCAT_CONFIG))// &
     &                      ' specified in the control file '//CONF_FILE )
                        RETURN
                   END IF
              END IF
              LMKDB_KEY = LMKDB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'MKDB.OUTPUT_NAME:' ) THEN
              PIM%CONF%MKDB_OUTPUT_NAME = BUF(J3)(IND(1,2):IND(2,2))
              LMKDB_KEY = LMKDB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'MKDB.DESC_FILE:' ) THEN
              CALL CLRCH ( STR )
              CALL TRAN ( 11, BUF(J3)(IND(1,2):IND(2,2)), STR )
              IF ( STR == 'NO' ) THEN
                   CALL CLRCH ( PIM%CONF%MKDB_DESC_FILE )
                 ELSE
                   PIM%CONF%MKDB_DESC_FILE = BUF(J3)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=PIM%CONF%MKDB_DESC_FILE, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 8247, IUER, 'PIMA_CONF', 'Cannot '// &
     &                      'find input MKDB.DESC_FILE file '// &
     &                      PIM%CONF%MKDB_DESC_FILE(1:I_LEN(PIM%CONF%MKDB_DESC_FILE))// &
     &                      ' specified in the control file '//CONF_FILE )
                        RETURN
                   END IF
              END IF
              LMKDB_KEY = LMKDB_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BPS.MODE:'     ) THEN
              IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__BPASS_INIT ) THEN
                   PIM%CONF%BPS_MODE = PIMA__BPASS_INIT
                ELSE  IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__BPASS_INSP ) THEN
                   PIM%CONF%BPS_MODE = PIMA__BPASS_INSP
                ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__BPASS_ACCUM ) THEN
                   PIM%CONF%BPS_MODE = PIMA__BPASS_ACCUM
                ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__BPASS_FINE  ) THEN
                   PIM%CONF%BPS_MODE = PIMA__BPASS_FINE
                ELSE
                   CALL ERR_LOG ( 8248, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword BPS.BPS_MODE: INIT, INSP, ACCUM, or FINE'// &
     &                 ' were expected' )
                   RETURN
              END IF
              LBPS_KEY = LBPS_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BPS.NOBS_ACCUM:'  ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I8)', IOSTAT=IER ) &
     &               PIM%CONF%BPS_NOBS_ACCUM
              IF ( IER .NE. 0 .OR. PIM%CONF%BPS_NOBS_ACCUM .LE. 0 ) THEN
                   CALL ERR_LOG ( 8249, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- an integer > 0 was expected' )
                   RETURN
              END IF
              LBPS_KEY = LBPS_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BPS.NOBS_FINE:'  ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I8)', IOSTAT=IER ) &
     &               PIM%CONF%BPS_NOBS_FINE
              IF ( IER .NE. 0 .OR. PIM%CONF%BPS_NOBS_FINE .LE. 0 ) THEN
                   CALL ERR_LOG ( 8250, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- an integer > 0 was expected' )
                   RETURN
              END IF
              LBPS_KEY = LBPS_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BPS.MINOBS_FINE:'  ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I8)', IOSTAT=IER ) &
     &               PIM%CONF%BPS_MINOBS_FINE
              IF ( IER .NE. 0 .OR. PIM%CONF%BPS_NOBS_FINE .LE. 0 ) THEN
                   CALL ERR_LOG ( 8251, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- an integer > 0 was expected' )
                   RETURN
              END IF
              LBPS_KEY = LBPS_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BPS.MSEG_ACCUM:'  ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I8)', IOSTAT=IER ) &
     &               PIM%CONF%BPS_MSEG_ACCUM
              IF ( IER .NE. 0 .OR. PIM%CONF%BPS_MSEG_ACCUM .LE. 0 ) THEN
                   CALL ERR_LOG ( 8252, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- an integer > 0 was expected' )
                   RETURN
              END IF
              LBPS_KEY = LBPS_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BPS.MSEG_FINE:'  ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I8)', IOSTAT=IER ) &
     &               PIM%CONF%BPS_MSEG_FINE
              IF ( IER .NE. 0 .OR. PIM%CONF%BPS_MSEG_FINE .LE. 0 ) THEN
                   CALL ERR_LOG ( 8253, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- an integer > 0 was expected' )
                   RETURN
              END IF
              LBPS_KEY = LBPS_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BPS.SNR_MIN_ACCUM:' ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F12.3)', IOSTAT=IER ) &
     &               PIM%CONF%BPS_SNR_MIN_ACCUM
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8254, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- a real number was expected' )
                   RETURN
              END IF
              LBPS_KEY = LBPS_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BPS.SNR_MIN_FINE:'  ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F12.3)', IOSTAT=IER ) &
     &               PIM%CONF%BPS_SNR_MIN_FINE
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8255, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- a real number was expected' )
                   RETURN
              END IF
              LBPS_KEY = LBPS_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BPS.DECOR_TIM_MIN:'  ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F12.3)', IOSTAT=IER ) &
     &               PIM%CONF%BPS_DECOR_TIM_MIN
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8256, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- a real number was expected' )
                   RETURN
              END IF
              LBPS_KEY = LBPS_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BPS.AMPL_REJECT:'  ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F9.3)', IOSTAT=IER ) &
     &               PIM%CONF%BPS_AMPL_REJECT
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8257, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- a real number was expected' )
                   RETURN
              END IF
              LBPS_KEY = LBPS_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BPS.PHAS_REJECT:'  ) THEN
              IF ( INDEX ( BUF(J3)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   BUF(J3)(IND(2,2)+1:IND(2,2)+2) = '.0'
                   IND(2,2) = IND(2,2) + 2
              END IF
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F9.3)', IOSTAT=IER ) &
     &               PIM%CONF%BPS_PHAS_REJECT
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8258, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- a real number was expected' )
                   RETURN
              END IF
              LBPS_KEY = LBPS_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BPS.INTRP_METHOD:'  ) THEN
              IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__LEGENDRE ) THEN
                   PIM%CONF%BPS_INTRP_METHOD = PIMA__LEGENDRE
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__CHEBYSH ) THEN
                   PIM%CONF%BPS_INTRP_METHOD = PIMA__CHEBYSH
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == 'CHEBYSHEV' ) THEN
                   PIM%CONF%BPS_INTRP_METHOD = PIMA__CHEBYSH
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__SPLINE ) THEN
                   PIM%CONF%BPS_INTRP_METHOD = PIMA__SPLINE 
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__LINEAR ) THEN
                   PIM%CONF%BPS_INTRP_METHOD = PIMA__LINEAR 
                 ELSE
                   CALL ERR_LOG ( 8259, IUER, 'PIMA_CONF', 'Error during '//&
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- of BPS.INTRP_METHOD. Supported methods are '// &
     &                 PIMA__LEGENDRE//', '//PIMA__CHEBYSH//', '// &
     &                 PIMA__SPLINE//', and '//PIMA__LINEAR )
                   RETURN
              END IF
              LBPS_KEY = LBPS_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BPS.DEG_AMP:'  ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I4)', IOSTAT=IER ) &
     &               PIM%CONF%BPS_DEG_AMP
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8260, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- a integer was expected' )
                   RETURN
                ELSE IF ( PIM%CONF%BPS_DEG_AMP >  PIM__MPOL ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( PIM__MPOL, STR )
                   CALL ERR_LOG ( 8261, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- too big degree of the amplitude bandpass '// &
     &                 'polynomial: greater than PIM__MPOL= '//STR )
                    RETURN
              END IF
              LBPS_KEY = LBPS_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BPS.DEG_PHS:'  ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I4)', IOSTAT=IER ) &
     &               PIM%CONF%BPS_DEG_PHS
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8262, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- a integer was expected' )
                   RETURN
                ELSE IF ( PIM%CONF%BPS_DEG_AMP >  PIM__MPOL ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( PIM__MPOL, STR )
                   CALL ERR_LOG ( 8263, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- too big degree of the phase bandpass '// &
     &                 'polynomial: greater than PIM__MPOL= '//STR )
                   RETURN
              END IF
              LBPS_KEY = LBPS_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BPS.AMP_MIN:'  ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F10.5)', IOSTAT=IER ) &
     &               PIM%CONF%BPS_AMP_MIN
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8264, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- a real number was expected' )
                   RETURN
              END IF
              LBPS_KEY = LBPS_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BPS.NORML:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+1) == PIMA__NORML_NO ) THEN
                   PIM%CONF%BPS_NORML = PIMA__NORML_NO
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+2) == PIMA__NORML_IF ) THEN
                   PIM%CONF%BPS_NORML = PIMA__NORML_IF
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+3) == PIMA__NORML_BAND ) THEN
                   PIM%CONF%BPS_NORML = PIMA__NORML_BAND
                 ELSE
                   CALL ERR_LOG ( 8265, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword BPS.NORML: NO, IF, or BAND '// &
     &                 'were expected' )
                   RETURN
              END IF
              LBPS_KEY = LBPS_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'BPS.SEFD_USE:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+1) == PIMA__BPASS_SEFD_NO ) THEN
                   PIM%CONF%BPS_SEFD_USE = PIMA__BPASS_SEFD_NO
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+2) == PIMA__BPASS_SEFD_YES ) THEN
                   PIM%CONF%BPS_SEFD_USE = PIMA__BPASS_SEFD_YES
                 ELSE
                   CALL ERR_LOG ( 8266, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' of keyword BPS.SEFD_USE: YES or NO '// &
     &                 'were expected' )
                   RETURN
              END IF
              LBPS_KEY = LBPS_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SPLT.SOU_NAME:'  ) THEN
              PIM%CONF%SPLT_SOU_NAME = BUF(J3)(IND(1,2):IND(2,2))
              LSPLT_KEY = LSPLT_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SPLT.FRQ_MSEG:'  ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I8)', IOSTAT=IER ) &
     &               PIM%CONF%SPLT_FRQ_MSEG
              IF ( IER .NE. 0 .OR. PIM%CONF%SPLT_FRQ_MSEG .LE. 0 ) THEN
                   CALL ERR_LOG ( 8267, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- an integer > 0 was expected' )
                   RETURN
              END IF
              LSPLT_KEY = LSPLT_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SPLT.TIM_MSEG:'  ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I8)', IOSTAT=IER ) &
     &               PIM%CONF%SPLT_TIM_MSEG
              IF ( IER .NE. 0 .OR. PIM%CONF%SPLT_TIM_MSEG .LE. 0 ) THEN
                   CALL ERR_LOG ( 8268, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- an integer > 0 was expected' )
                   RETURN
              END IF
              LSPLT_KEY = LSPLT_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SPLT.WEIGHT_TYPE:'  ) THEN
              IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__SPLT_WEI_ONE ) THEN
                   PIM%CONF%SPLT_WEIGHT_TYPE = PIMA__SPLT_WEI_ONE 
                ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__SPLT_WEI_OBS_SNR ) THEN
                   PIM%CONF%SPLT_WEIGHT_TYPE = PIMA__SPLT_WEI_OBS_SNR 
                ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__SPLT_WEI_OBS_RMS ) THEN
                   PIM%CONF%SPLT_WEIGHT_TYPE = PIMA__SPLT_WEI_OBS_RMS 
                ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__SPLT_WEI_SEG_RMS ) THEN
                   PIM%CONF%SPLT_WEIGHT_TYPE = PIMA__SPLT_WEI_SEG_RMS 
                ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__SPLT_WEI_AUTO ) THEN
                   PIM%CONF%SPLT_WEIGHT_TYPE = PIMA__SPLT_WEI_AUTO 
                ELSE 
                   CALL ERR_LOG ( 8269, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- supported values: ONE, OBS_SNR, OBS_RMS, SEG_RMS, '//&
     &                 'or AUTO' )
                   RETURN
              END IF
              LSPLT_KEY = LSPLT_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SPLT.POLAR:'  ) THEN
              PIM%CONF%SPLT_POLAR = BUF(J3)(IND(1,2):IND(2,2))
              IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_RR ) THEN
                   PIM%CONF%SPLT_POLAR = PIMA__POLAR_RR
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_LL ) THEN
                   PIM%CONF%SPLT_POLAR = PIMA__POLAR_LL
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_RL ) THEN
                   PIM%CONF%SPLT_POLAR = PIMA__POLAR_RL
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_LR ) THEN
                   PIM%CONF%SPLT_POLAR = PIMA__POLAR_LR
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_I ) THEN
                   PIM%CONF%SPLT_POLAR = PIMA__POLAR_I
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_PAR  ) THEN
                   PIM%CONF%SPLT_POLAR = PIMA__POLAR_PAR
                 ELSE IF ( BUF(J3)(IND(1,2):IND(1,2)+7) == PIMA__POLAR_ALL  ) THEN
                   PIM%CONF%SPLT_POLAR = PIMA__POLAR_ALL
                 ELSE
                   CALL ERR_LOG ( 8270, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong value of the keyword SPLT.POLAR.  '// &
     &                 'Code: '//BUF(J3)(IND(1,2):IND(1,2)+7)//' is not supported. '// &
     &                 ' Supported codes: RR, RL, LR, LL, PAR, ALL' )
                   RETURN
              END IF
              LSPLT_KEY = LSPLT_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SPLT.AUTOCORR_NRML_METHOD:' ) THEN
              IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__NORML_NO ) THEN
                   PIM%CONF%SPLT_AUTOCORR_NRML_METHOD = PIMA__NORML_NO
                ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__AVERAGED ) THEN
                   PIM%CONF%SPLT_AUTOCORR_NRML_METHOD = PIMA__AVERAGED 
                ELSE
                   CALL ERR_LOG ( 8271, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong value of the keyword SPLT.AUTOCORR_NRML_METHOD. '// &
     &                 'Value: '//BUF(J3)(IND(1,2):IND(2,2))//' is not supported. '// &
     &                 ' Supported values: NO, AVERAGED' )
                   RETURN
              END IF
              LSPLT_KEY = LSPLT_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SPLT.BPASS_NRML_METHOD:'    ) THEN
              IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__NORML_NO ) THEN
                   PIM%CONF%SPLT_BPASS_NRML_METHOD = PIMA__NORML_NO
                ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__WEIGHTED ) THEN
                   PIM%CONF%SPLT_BPASS_NRML_METHOD = PIMA__WEIGHTED
                ELSE
                   CALL ERR_LOG ( 8272, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong value of the keyword SPLT.BPASS_NRML_METHOD. '// &
     &                 'Value: '//BUF(J3)(IND(1,2):IND(2,2))//' is not supported. '// &
     &                 ' Supported values: NO, WEIGHTED' )
                   RETURN
              END IF
              LSPLT_KEY = LSPLT_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SPLT.BPASS_NRML_RANGE:'     ) THEN
              STR = BUF(J3)(IND(1,2):IND(2,2))
              IP = INDEX ( STR, ':' )
              IF ( IP < 1 ) IP = INDEX ( STR, ',' )
              IF ( IP < 1 .OR. IP == 1 .OR. IP == ILEN(STR) )  THEN
                   CALL ERR_LOG ( 8273, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong value of the keyword SPLT.BPASS_NRML_RANGE: '// &
     &                 BUF(J3)(IND(1,2):IND(2,2))//'. Two real values '// &
     &                 'separated by column were expected' )
                   RETURN
              END IF
!
              STR1 = STR(1:IP-1)
              IF ( INDEX( STR1, '.' ) == 0 ) STR1 = STR1(1:I_LEN(STR1))//'.0'
              READ ( UNIT=STR1 , FMT='(F10.5)', IOSTAT=IER ) PIM%CONF%SPLT_BPASS_NRML_RANGE(1)
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8274, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong value of the keyword SPLT.BPASS_NRML_RANGE: '// &
     &                 BUF(J3)(IND(1,2):IND(2,2))//'. Cannot parse the first '// &
     &                 'value: a real number was expected, but got '//STR(1:IP-1) )
                   RETURN
              END IF
!
              STR1 = STR(IP+1:)
              IF ( INDEX( STR1, '.' ) == 0 ) STR1 = STR1(1:I_LEN(STR1))//'.0'
              READ ( UNIT=STR1, FMT='(F10.5)', IOSTAT=IER ) PIM%CONF%SPLT_BPASS_NRML_RANGE(2)
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8275, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong value of the keyword SPLT.BPASS_RENRML: '// &
     &                 BUF(J3)(IND(1,2):IND(2,2))//'. Cannot parse the second '// &
     &                 'value: a real number was expected, but got '//STR(IP+1:) )
                   RETURN
              END IF
              LSPLT_KEY = LSPLT_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SPLT.STA_BASED:'  ) THEN
              PIM%CONF%SPLT_STA_BASED = BUF(J3)(IND(1,2):IND(2,2))
              IF ( PIM%CONF%SPLT_STA_BASED == PIMA__STA_BASED_ALL ) THEN
                   CONTINUE 
                 ELSE IF ( PIM%CONF%SPLT_STA_BASED == PIMA__STA_BASED_YES ) THEN
                   CONTINUE 
                 ELSE IF ( PIM%CONF%SPLT_STA_BASED == PIMA__STA_BASED_NO ) THEN
                   CONTINUE 
                 ELSE 
                   CALL ERR_LOG ( 8276, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong value of the keyword SPLT.STA_BASED '// &
     &                 BUF(J3)(IND(1,2):IND(1,2)+7)//'. Supported value: '// &
     &                'ALL or YES or NO' )
                   RETURN
              END IF
              LSPLT_KEY = LSPLT_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SPLT.TOTAL_UV:'  ) THEN
              PIM%CONF%SPLT_TOTAL_UV = BUF(J3)(IND(1,2):IND(2,2))
              IF ( PIM%CONF%SPLT_TOTAL_UV == PIMA__TOTAL_UV_YES ) THEN
                   CONTINUE 
                 ELSE IF ( PIM%CONF%SPLT_STA_BASED == PIMA__TOTAL_UV_YES ) THEN
                   CONTINUE 
                 ELSE 
                   CALL ERR_LOG ( 8277, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong value of the keyword SPLT.TOTAL_UV '// &
     &                 BUF(J3)(IND(1,2):IND(1,2)+7)//'. Supported value: '// &
     &                'are YES or NO' )
                   RETURN
              END IF
              LSPLT_KEY = LSPLT_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SPLT.SNR_MIN:'  ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F10.5)', IOSTAT=IER ) &
     &               PIM%CONF%SPLT_SNR_MIN
              IF ( IER .NE. 0 .OR. PIM%CONF%SPLT_SNR_MIN < -1.0 ) THEN
                   CALL ERR_LOG ( 8278, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- unrecognized value '//BUF(J3)(IND(1,2):IND(2,2))// &
     &                 ' -- an real number > 0 was expected' )
                   RETURN
              END IF
              LSPLT_KEY = LSPLT_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SPLT.GAIN_CORR_FILE:'  ) THEN
              PIM%CONF%SPLT_GAIN_CORR_FILE = BUF(J3)(IND(1,2):IND(2,2))
              CALL CLRCH ( STR )
              CALL TRAN ( 11, BUF(J3)(IND(1,2):IND(2,2)), STR )
              IF ( STR == 'NO' ) THEN
                   CALL CLRCH ( PIM%CONF%SPLT_GAIN_CORR_FILE )
                   IF ( PIM%CONF%ACT_CODE == PIMA__GACO_CODE ) THEN
                        CALL ERR_LOG ( 8279, IUER, 'PIMA_CONF', 'Gain correction '// &
     &                      'file should be specified in the keyword '// &
     &                      'SPLT.GAIN_CORR_FILE in order to run task gaco' )
                        RETURN
                   END IF
                 ELSE
                   PIM%CONF%SPLT_GAIN_CORR_FILE = BUF(J3)(IND(1,2):IND(2,2))
                   INQUIRE ( FILE=PIM%CONF%SPLT_GAIN_CORR_FILE, EXIST=LEX )
                   IF ( .NOT. LEX .AND. PIM%CONF%ACT_CODE == PIMA__SPLT_CODE ) THEN
                        CALL ERR_LOG ( 8280, IUER, 'PIMA_CONF', 'Cannot '// &
     &                      'find input SPLT.GAIN_CORR_FILE file '// &
     &                      PIM%CONF%SPLT_GAIN_CORR_FILE(1:I_LEN(PIM%CONF%SPLT_GAIN_CORR_FILE))// &
     &                      ' specified in the control file '//CONF_FILE )
                        RETURN
                   END IF
              END IF
              LSPLT_KEY = LSPLT_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SPLT.SUBARRY_CONSOLIDATION:'  ) THEN
              IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__SA_NO ) THEN
                   PIM%CONF%SPLT_SUBARRAY_CONSOLIDATION = PIMA__SA_NO
                ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__SA_MIN ) THEN
                   PIM%CONF%SPLT_SUBARRAY_CONSOLIDATION = PIMA__SA_MIN
                ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__SA_MAX ) THEN
                   PIM%CONF%SPLT_SUBARRAY_CONSOLIDATION = PIMA__SA_MAX
                ELSE
                   CALL ERR_LOG ( 8281, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong value of the keyword SPLT.SUBARRY_CONSOLIDATION. '// &
     &                 'Value: '//BUF(J3)(IND(1,2):IND(2,2))//' is not supported. '// &
     &                 ' Supported values: NO, MIN, MAX' )
                   RETURN
              END IF
              LSPLT_KEY = LSPLT_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'ONOF.GEN_FLAGS_MODE:'  ) THEN
              IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__ONOF_CREATE ) THEN
                   PIM%CONF%ONOF_GEN_FLAGS_MODE = PIMA__ONOF_CREATE
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__ONOF_UPDATE ) THEN
                   PIM%CONF%ONOF_GEN_FLAGS_MODE = PIMA__ONOF_UPDATE
                 ELSE IF ( BUF(J3)(IND(1,2):IND(2,2)) == PIMA__ONOF_NO ) THEN
                   PIM%CONF%ONOF_GEN_FLAGS_MODE = PIMA__ONOF_NO
                 ELSE 
                   CALL ERR_LOG ( 8282, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong value of the keyword ONOF.GEN_FLAGS_MODE '// &
     &                 BUF(J3)(IND(1,2):IND(1,2)+7)//'. Supported value: '// &
     &                'CREATE, UPDATE or NO' )
                   RETURN
              END IF
              LONOF_KEY = LONOF_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'ONOF.AMPL_THRESHOLD:'  ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F12.5)', IOSTAT=IER ) &
     &               PIM%CONF%ONOF_AMPL_THRESHOLD
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8283, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong value of the keyword ONOF.AMPL_THRESHOLD '// &
     &                 BUF(J3)(IND(1,2):IND(1,2)+7)//', while a non-negative '// &
     &                'real number was expected' )
                   RETURN
              END IF
              LONOF_KEY = LONOF_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'ONOF.NSIG_THRESHOLD:'  ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F12.5)', IOSTAT=IER ) &
     &               PIM%CONF%ONOF_NSIG_THRESHOLD 
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8284, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong value of the keyword ONOF.NSIG_THRESHOLD '// &
     &                 BUF(J3)(IND(1,2):IND(1,2)+7)//', while a non-negative '// &
     &                'real number was expected' )
                   RETURN
              END IF
              LONOF_KEY = LONOF_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'ONOF.COHERENT_INTERVAL:'  ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F12.5)', IOSTAT=IER ) &
     &               PIM%CONF%ONOF_COHERENT_INTERVAL
              IF ( IER .NE. 0 .OR. PIM%CONF%ONOF_COHERENT_INTERVAL .LE. 0.0 ) THEN
                   CALL ERR_LOG ( 8285, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong value of the keyword ONOF.COHERENT_INTERVAL '// &
     &                 BUF(J3)(IND(1,2):IND(1,2)+7)//', while a positive '// &
     &                'real number was expected' )
                   RETURN
              END IF
              LONOF_KEY = LONOF_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'ONOF.MIN_LOW_AP:'  ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(I9)', IOSTAT=IER ) &
     &               PIM%CONF%ONOF_MIN_LOW_AP
              IF ( IER .NE. 0 .OR. PIM%CONF%ONOF_MIN_LOW_AP .LE. 0 ) THEN
                   CALL ERR_LOG ( 8286, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong value of the keyword ONOF.MIN_LOW_AP: '// &
     &                 BUF(J3)(IND(1,2):IND(1,2)+7)//', while a positive '// &
     &                'integer number was expected' )
                   RETURN
              END IF
              LONOF_KEY = LONOF_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'ONOF.KERNEL_START_SHARE:'  ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F12.5)', IOSTAT=IER ) &
     &               PIM%CONF%ONOF_KERNEL_START_SHARE
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8287, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong value of the keyword ONOF.KERNEL_START_SHARE '// &
     &                 BUF(J3)(IND(1,2):IND(1,2)+7)//', while a real '// &
     &                'number in range [0.0, 1.0] was expected' )
                   RETURN
              END IF
              LONOF_KEY = LONOF_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'ONOF.KERNEL_END_SHARE:'  ) THEN
              READ ( UNIT=BUF(J3)(IND(1,2):IND(2,2)), FMT='(F12.5)', IOSTAT=IER ) &
     &               PIM%CONF%ONOF_KERNEL_END_SHARE
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8288, IUER, 'PIMA_CONF', 'Error during '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &                 'control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- Wrong value of the keyword ONOF.KERNEL_END_SHARE '// &
     &                 BUF(J3)(IND(1,2):IND(1,2)+7)//', while a real '// &
     &                'number in range [0.0, 1.0] was expected' )
                   RETURN
              END IF
              LONOF_KEY = LONOF_KEY + 1
            ELSE IF ( BUF(J3)(IND(1,1):IND(1,1)+7) == 'PIMAVAR_' ) THEN
!
! ------------ This keyword is a PIMA-recognized environment variable
!
               IF ( BUF(J3)(IND(2,1):IND(2,1)) == ':' ) THEN
                    IND(2,1) = IND(2,1) - 1
               END IF
               STR = BUF(J3)(IND(1,1):IND(2,1))//'='// &
     &               BUF(J3)(IND(1,LIND):IND(2,LIND))
               IS = SETENV ( BUF(J3)(IND(1,1):IND(2,1))//CHAR(0), &
     &                       BUF(J3)(IND(1,2):IND(2,LIND))//CHAR(0), &
     &                       %VAL(0) )
               IF ( IS .NE. 0 ) THEN
                   CALL ERR_LOG ( 8289, IUER, 'PIMA_CONF', 'Error in '// &
     &                 'an attempt to assigm environoment variable '// &
     &                  BUF(J3)(IND(1,1):IND(2,1))//' value '// &
     &                  BUF(J3)(IND(1,2):IND(2,LIND)) )
                   RETURN
               END IF
            ELSE
              CALL ERR_LOG ( 8290, IUER, 'PIMA_CONF', 'Error during parsing '// &
     &            'the '//STR(1:I_LEN(STR))//'-th line of the control file '// &
     &             CONF_FILE(1:I_LEN(CONF_FILE))//' -- "'// &
     &             BUF(J3)(1:I_LEN(BUF(J3)))//'" -- unrecognized keyword' )
              RETURN
         END IF
 430  CONTINUE
!
! --- Check whether all general keywods were found
!
      IF ( LKEY < PIMA__MKEY ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LKEY, STR )
           CALL INCH  ( PIMA__MKEY, STR1 )
           CALL ERR_LOG ( 8291, IUER, 'PIMA_CONF', 'Not all keywords were '// &
     &         'found in the control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &         ' -- only '//STR(1:I_LEN(STR))//' out of '//STR1 )
           RETURN
      END IF
!
      IF ( LFRIB_KEY < PIMA__FRIB_KEY ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LFRIB_KEY, STR )
           CALL INCH  ( PIMA__FRIB_KEY, STR1 )
           CALL ERR_LOG ( 8292, IUER, 'PIMA_CONF', 'Not all keywords '// &
     &         'for FRIB section were found in the control file '// &
     &          CONF_FILE(1:I_LEN(CONF_FILE))// &
     &         ' -- only '//STR(1:I_LEN(STR))//' out of '//STR1 )
           RETURN
      END IF
!
      IF ( LFRIP_KEY < PIMA__FRIP_KEY ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LFRIB_KEY, STR )
           CALL INCH  ( PIMA__FRIP_KEY, STR1 )
           CALL ERR_LOG ( 8293, IUER, 'PIMA_CONF', 'Not all keywords '// &
     &         'for FRIP section were found in the control file '// &
     &          CONF_FILE(1:I_LEN(CONF_FILE))// &
     &         ' -- only '//STR(1:I_LEN(STR))//' out of '//STR1 )
           RETURN
      END IF
!
      IF ( LMKDB_KEY < PIMA__MKDB_KEY ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LMKDB_KEY, STR )
           CALL INCH  ( PIMA__MKDB_KEY, STR1 )
           CALL ERR_LOG ( 8294, IUER, 'PIMA_CONF', 'Not all keywords '// &
     &         'for MKDB were found in the control file '// &
     &          CONF_FILE(1:I_LEN(CONF_FILE))// &
     &         ' -- only '//STR(1:I_LEN(STR))//' out of '//STR1 )
           RETURN
      END IF
!
      IF ( LBPS_KEY < PIMA__BPS_KEY ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LBPS_KEY, STR )
           CALL INCH  ( PIMA__BPS_KEY, STR1 )
           CALL ERR_LOG ( 8295, IUER, 'PIMA_CONF', 'Not all keywords '// &
     &         'for BPS were found in the control file '// &
     &          CONF_FILE(1:I_LEN(CONF_FILE))// &
     &         ' -- only '//STR(1:I_LEN(STR))//' out of '//STR1 )
           RETURN
      END IF
!
      IF ( LSPLT_KEY < PIMA__SPLT_KEY ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LSPLT_KEY, STR )
           CALL INCH  ( PIMA__SPLT_KEY, STR1 )
           CALL ERR_LOG ( 8296, IUER, 'PIMA_CONF', 'Not all keywords '// &
     &         'for SPLT were found in the control file '// &
     &          CONF_FILE(1:I_LEN(CONF_FILE))// &
     &         ' -- only '//STR(1:I_LEN(STR))//' out of '//STR1 )
           RETURN
      END IF
!
      IF ( LONOF_KEY < PIMA__ONOF_KEY ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LONOF_KEY, STR )
           CALL INCH  ( PIMA__ONOF_KEY, STR1 )
           CALL ERR_LOG ( 8297, IUER, 'PIMA_CONF', 'Not all keywords '// &
     &         'for ONOF were found in the control file '// &
     &          CONF_FILE(1:I_LEN(CONF_FILE))// &
     &         ' -- only '//STR(1:I_LEN(STR))//' out of '//STR1 )
           RETURN
      END IF
!
      IF ( PIM%CONF%MKDB_OUTPUT_TYPE == 'GVF' ) THEN
           PIM%CONF%MKDB_SUFFIX = PIM%CONF%MKDB_OUTPUT_NAME
           CALL CLRCH ( PIM%CONF%MKDB_OUTPUT_NAME )
      END IF
!
! --- Check parameters compatibility
!
      IF ( PIM%CONF%ACT_CODE == PIMA__FRIB_CODE .AND. &
     &     ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_MUL  .OR.     &
     &       PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_ADD  .OR.     &
     &       PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_LSQ           ) .AND. &
     &       PIM%CONF%FRIB_FINE_SEARCH .NE. PIMA__FINE_SEARCH_ACC        .AND. &
     &       PIM%CONF%FRIB_FINE_SEARCH .NE. PIMA__FINE_SEARCH_LSQ         ) THEN
!
             CALL ERR_LOG ( 8298, IUER, 'PIMA_CONF', 'Keywords '// &
     &           'MKDB.FRINGE_ALGORITHM '//PIM%CONF%MKDB_FRINGE_ALGORITHM// &
     &           ' and FRIB.FINE_SEARCH '//PIM%CONF%FRIB_FINE_SEARCH// &
     &           ' are not compatible' )
             RETURN
      END IF
!
      IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE .AND. PIM%CONF%BPS_DEG_AMP < 3 ) THEN
           CALL ERR_LOG ( 8299, IUER, 'PIMA_CONF', 'Wrong value of '// &
     &         'BPS.DEG_AMP -- when BPS.INTRP_METHOD: SPLINE is chosen '// &
     &         'the minimum bumber of interpolation knots is 3. Please '// &
     &         'increase the value of BPS.DEG_AMP' )
           RETURN
      END IF
!
      IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE .AND. PIM%CONF%BPS_DEG_PHS < 3 ) THEN
           CALL ERR_LOG ( 8300, IUER, 'PIMA_CONF', 'Wrong value of '// &
     &         'BPS.DEG_PHS -- when BPS.INTRP_METHOD: SPLINE is chosen '// &
     &         'the minimum bumber of interpolation knots is 3. Please '// &
     &         'increase the value of BPS.DEG_PHS' )
           RETURN
      END IF
!
      IF ( PIM%CONF%PHASE_ACCELERATION .NE. 0.0D0 .AND. &
     &     ( PIM%CONF%PHASE_ACCEL_MIN .NE. 0.0D0 .OR. &
     &       PIM%CONF%PHASE_ACCEL_MAX .NE. 0.0D0      ) ) THEN
            CALL ERR_LOG ( 8301, IUER, 'PIMA_CONF', 'Unsupported '// &
     &          'combination of parameters: if PIM%CONF%PHASE_ACCELERATION '// &
     &          'is non-zero, then PIM%CONF%PHASE_ACCEL_MIN and '// &
     &          'PIM%CONF%PHASE_ACCEL_MIN must be zero ' )
            RETURN
      END IF 
!
      IF ( PIM%CONF%PHASE_ACCEL_MAX < PIM%CONF%PHASE_ACCEL_MIN ) THEN
           CALL ERR_LOG ( 8302, IUER, 'PIMA_CONF', 'Unsupported combination '// &
     &         'of parameters: if PIM%CONF%PHASE_ACCEL_MIN should be less '// &
     &         'than PIM%CONF%PHASE_ACCEL_MAX ' )
           RETURN
      END IF 
!
      IF ( PIM%CONF%ACT_CODE .NE. PIMA__BPAS_CODE .AND. &
     &     PIM%CONF%ACT_CODE .NE. PIMA__PCPL_CODE .AND. &
     &     PIM%CONF%ACT_CODE .NE. PIMA__PDPL_CODE .AND. &
     &     PIM%CONF%ACT_CODE .NE. PIMA__MPPL_CODE .AND. &
     &     PIM%CONF%ACT_CODE .NE. PIMA__TSPL_CODE .AND. &
     &     PIM%CONF%ACT_CODE .NE. PIMA__PMGE_CODE .AND. &
     &     PIM%CONF%ACT_CODE .NE. PIMA__BMGE_CODE .AND. &
     &     PIM%CONF%ACT_CODE .NE. PIMA__OPAG_CODE .AND. &
     &     PIM%CONF%ACT_CODE .NE. PIMA__OPAL_CODE .AND. &
     &     PIM%CONF%ACT_CODE .NE. PIMA__ACPL_CODE .AND. &
     &     PIM%CONF%ACT_CODE .NE. PIMA__CLPC_CODE .AND. &
     &     PIM%CONF%ACT_CODE .NE. PIMA__GEPM_CODE       ) THEN
!
! -------- Check whether file with polarization bandpass exists
!
           IF ( PIM%CONF%POLARCAL_FILE .NE. PIMA__POLARCAL_NO ) THEN
                INQUIRE ( FILE=PIM%CONF%POLARCAL_FILE, EXIST=LEX )
                IF ( .NOT. LEX ) THEN
                     CALL ERR_LOG ( 8303, IUER, 'PIMA_CONF', 'Cannot '// &
     &                   'find input POLARCAL_FILE file '// &
     &                   PIM%CONF%POLARCAL_FILE(1:I_LEN(PIM%CONF%POLARCAL_FILE))// &
     &                   ' specified in the control file '//CONF_FILE )
                     RETURN
                END IF
           END IF
      END IF
!
      IF ( PIM%CONF%SPLT_WEIGHT_TYPE == PIMA__SPLT_WEI_SEG_RMS .AND. &
     &     PIM%CONF%SPLT_TIM_MSEG < PIM__SPLT_SNR_MINSEG             ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( PIM%CONF%SPLT_TIM_MSEG, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( PIM__SPLT_SNR_MINSEG,   STR1 )
           CALL ERR_LOG ( 8304, IUER, 'PIMA_CONF', 'Conflict of parameters '// &
     &         'SPLT.TIM_MSEG: '//STR(1:I_LEN(STR))//' and SPLT.WEIGHT_TYPE: '// &
     &         '= AUTO. SPLT.TIM_MSEG should be no less than internal PIMA '// &
     &         'parameter PIM__SPLT_SNR_MINSEG equal to '//STR1 )
           RETURN
      END IF
!
      IF ( PIM%CONF%ONOF_KERNEL_START_SHARE < 0.0 .OR. PIM%CONF%ONOF_KERNEL_START_SHARE > 1.0 ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR(1:12), FMT='(F12.5)' ) PIM%CONF%ONOF_KERNEL_START_SHARE 
           CALL ERR_LOG ( 8305, IUER, 'PIMA_CONF', 'Wrong value of parameter '// &
     &         'ONOF.KERNEL_START_SHARE: '//STR(1:I_LEN(STR))//' -- a real number '// &
     &         'in a range [0.0, 1.0] was expected' )
           RETURN
      END IF
      IF ( PIM%CONF%ONOF_KERNEL_END_SHARE < 0.0 .OR. PIM%CONF%ONOF_KERNEL_END_SHARE > 1.0 ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR(1:12), FMT='(F12.5)' ) PIM%CONF%ONOF_KERNEL_END_SHARE 
           CALL ERR_LOG ( 8306, IUER, 'PIMA_CONF', 'Wrong value of parameter '// &
     &         'ONOF.KERNEL_END_SHARE: '//STR(1:I_LEN(STR))//' -- a real number '// &
     &         'in a range [0.0, 1.0] was expected' )
           RETURN
      END IF
      IF ( PIM%CONF%ONOF_KERNEL_END_SHARE .LE. PIM%CONF%ONOF_KERNEL_START_SHARE ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR(1:12), FMT='(F12.5)' ) PIM%CONF%ONOF_KERNEL_END_SHARE 
           CALL CLRCH ( STR1 )
           WRITE ( UNIT=STR1(1:12), FMT='(F12.5)' ) PIM%CONF%ONOF_KERNEL_START_SHARE 
           CALL ERR_LOG ( 8307, IUER, 'PIMA_CONF', 'Wrong value of parameter '// &
     &         'ONOF.KERNEL_END_SHARE: '//STR(1:I_LEN(STR))//' -- it should be '// &
     &         'then ONOF.KERNEL_START_SHARE: '//STR1 )
           RETURN
      END IF
!
      IF ( PIM%CONF%ACT_CODE == PIMA__ONOF_CODE ) THEN
           IF ( PIM%CONF%ONOF_GEN_FLAGS_MODE == PIMA__ONOF_CREATE .OR. &
     &          PIM%CONF%ONOF_GEN_FLAGS_MODE == PIMA__ONOF_UPDATE      ) THEN
                CONTINUE 
              ELSE
                CALL ERR_LOG ( 8308, IUER, 'PIMA_CONF', 'Wrong keyword '// &
     &              'ONOF.GEN_FLAGS_MODE: '//PIM%CONF%ONOF_GEN_FLAGS_MODE// &
     &              '. Task onof requires UPDATE or CREATE mode' )
                RETURN
           END IF
      END IF
!
      IF ( PIM%CONF%FRQ_GRP < 1 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( PIM%CONF%FRQ_GRP, STR )
           CALL ERR_LOG ( 8309, IUER, 'PIMA_CONF', 'Wrong keyword '// &
     &         'FRQ_GRP: '//STR(1:I_LEN(STR))//' -- the frequency group '// &
     &         'index should be a positive integer' )
           RETURN
      END IF
!
      IF ( PIM%CONF%BEG_FRQ < 1 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( PIM%CONF%BEG_FRQ, STR )
           CALL ERR_LOG ( 8310, IUER, 'PIMA_CONF', 'Wrong keyword '// &
     &         'BEG_FRQ: '//STR(1:I_LEN(STR))//' -- the beginning '// &
     &         'frequency index should be a positive integer' )
           RETURN
      END IF
!
      IF ( PIM%CONF%END_FRQ < PIM%CONF%BEG_FRQ ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( PIM%CONF%END_FRQ, STR )
           CALL ERR_LOG ( 8311, IUER, 'PIMA_CONF', 'Wrong keyword '// &
     &         'END_FRQ: '//STR(1:I_LEN(STR))//' -- the ending '// &
     &         'frequency index should be no less than the beginning '// &
     &         'frequency index' )
           RETURN
      END IF
!
      IF ( PIM%CONF%ACT_CODE == PIMA__BPAS_CODE ) THEN
           IF ( PIM%CONF%POLARCAL_FILE .NE. PIMA__POLARCAL_NO .AND. &
     &          PIM%CONF%POLAR .NE. PIMA__POLAR_I                 ) THEN
                CALL ERR_LOG ( 8312, IUER, 'PIMA_CONF', 'Wrong combination '// &
     &              'of keywords: you have specified no polarization bandpass '// &
     &              'and polarization '//TRIM(PIM%CONF%POLAR)//'. If you want to '// &
     &              'compute polarization bandpass, please specify it with '// &
     &              'keyword POLARCAL_FILE. If you do not want to compute '// &
     &              'the polarization bandpass, please specify POLAR: RR or '// &
     &              'LL or HH or VV, whichever is suitable for this experiment' )
                RETURN 
           END IF
      END IF
!
      IF ( (  PIM%CONF%ACT_CODE .EQ. PIMA__SPLT_CODE .OR. &
     &        PIM%CONF%ACT_CODE .EQ. PIMA__GACO_CODE      ) .AND. &
     &     ( PIM%CONF%SPLT_POLAR == PIMA__POLAR_ALL .OR. &
     &       PIM%CONF%SPLT_POLAR == PIMA__POLAR_I   .OR. &
     &       PIM%CONF%SPLT_POLAR == PIMA__POLAR_PAR      ) .AND. &
     &       PIM%CONF%POLAR .NE. PIMA__POLAR_I                   ) THEN
             CALL ERR_LOG ( 8313, IUER, 'PIMA_CONF', 'Wrong combination '// &
     &              'of keywords: SPLT.POLAR: I or ALL or PAR require '// &
     &              'POLAR: I' )
             RETURN 
      END IF
!
      PIM%L_FIL = PIM%CONF%L_FIL
!
! --- Now allocate memory for data structures associated with UV FITS files...
!
      ALLOCATE ( PIM%FILE(PIM%L_FIL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM__MFIL*SIZEOF(PIM%FILE(1)), STR )
           CALL ERR_LOG ( 8314, IUER, 'PIMA_CONF', 'Failure to allocate '// &
    &          STR(1:I_LEN(STR))//' bytes of dynamic memory for PIMA '// &
     &         'internal data structures for keeping inforamtion about '// &
     &         'UV-files and their keywords. What is going on? Do you '// &
     &         'really have so few memory?' )
           CALL EXIT ( 1 )
      END IF
!
! --- ... and copy file names there
!
      DO 4120 J12=1,PIM%L_FIL
         CALL NOUT ( SIZEOF(PIM%FILE(J12)), PIM%FILE(J12) )
         PIM%FILE(J12)%NAME      = PIM%CONF%UVFILE_NAME(J12)
         PIM%FILE(J12)%ORIG_NAME = PIM%CONF%UVFILE_NAME(J12)
 4120 CONTINUE
!
      CALL NOUT ( PIM__FILL, PIM%CONF%FILLER )
      PIM%CONF_FILE = CONF_FILE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_CONF  !#!#
