      SUBROUTINE OPA_STSINQ ( IVRB, OPA, OPC_FILE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  OPA_STSINQ  inquires status of processing the database    *
! *   OPA.DB_NAME . It sets the status arrays OPA.STS and then updates   *
! *   operational analysis control file.                                 *
! *                                                                      *
! *   OPA_STSINQ performs the following checks:                          *
! *                                                                      *
! *       1) Whether the database has been submitted to IVS Data Center? *
! *       2) Whether the database is in the global arc-list?             *
! *       3) Whether the databases has been transformed to superfile?    *
! *       4) Whether baseline-dependent weights has been computed?       *
! *       5) Whether site-dependent weights has been computed?           *
! *       6) Whether all stations and sources are in the mapping file?   *
! *       7) Whether the databases contributed to computation of EOP and *
! *          there is the entry in EOB-file?                             *
! *       8) Whether listing of session-type loose solution in SINEX     *
! *          format has been produced?                                   *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  OPC_FILE ( CHARACTER ) -- Operational analysis control file.        *
! *       OPA ( RECORD    ) -- Data structure which keeps settings       *
! *                            of OPA and current information related    *
! *                            to processing this session.               *
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
! *  ### 15-AUG-2000   OPA_STSINQ  v1.7 (c)  L. Petrov  28-FEB-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'precm.i'
      INCLUDE    'opa.i'
      INTEGER*4  IVRB, IUER
      CHARACTER  OPC_FILE*(*)
      TYPE ( OPA__STRU ) ::  OPA
      INTEGER*4  M_SOU, M_STA
      PARAMETER  ( M_SOU = MAX_SRC )     ! Max number of arc sources
      PARAMETER  ( M_STA = MAX_ARC_STA ) ! Max number of arc stations
      LOGICAL*4  LCH, CHECK_IVSDIR, GLOARC_CHECK
      CHARACTER  CLIS_SOU(M_SOU)*8, CLIS_STA(M_STA)*8, OUT*2048
      INTEGER*4  IVER, L_STA, L_SOU, IPOS_WGT_BAS, IPOS_WGT_SIT, IP, IER
      REAL*8     DURATION
      INTEGER*4, EXTERNAL :: I_LEN, LINDEX
!
! --- 1) Check IVS Data Center: whether databases has been submitted?
!
      CALL ERR_PASS ( IUER, IER )
      LCH = CHECK_IVSDIR ( OPA%DB_NAME, OPA%IVS_DB_URL, OPA%TMP_DIR, &
     &                     OPA%WGET_EXE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4231, IUER, 'OPA_STSINQ', 'Failure to read '// &
     &         'directory from IVS web-site and find there experiment '// &
     &          OPA%DB_NAME )
           RETURN
      END IF
      IF ( LCH ) THEN
           OPA%STS(OPA__SBD) = '+'
         ELSE
           OPA%STS(OPA__SBD) = '-'
      END IF
      IF ( OPA%STS(OPA__SBD) .EQ. '-' ) OPA%ACT(OPA__SBD) = '?'
!
! --- 2) Check global arc-list. Whether it includes the experiment?
!
      CALL ERR_PASS ( IUER, IER )
      LCH = GLOARC_CHECK ( OPA%DB_NAME, OPA%DB_VERSION, OPA%GLO_ARC_FILE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4232, IUER, 'OPA_STSINQ', 'Failure to read '// &
     &         'global arc-file '//OPA%GLO_ARC_FILE(1:I_LEN(OPA%GLO_ARC_FILE))// &
     &         ' and to learn whether superfile for experiment '// &
     &         OPA%DB_NAME//' is there' )
           RETURN
      END IF
      IF ( LCH ) THEN
           OPA%STS(OPA__GAL) = '+'
         ELSE
           OPA%STS(OPA__GAL) = '-'
      END IF
      IF ( OPA%STS(OPA__GAL) .EQ. '-' ) OPA%ACT(OPA__GAL) = '?'
!
! --- 3) Check superfile catalogue. Whether the database was reformeted
! --- to superfile?
!
      CALL ERR_PASS ( IUER, IER )
      CALL SUPCAT_CHECK ( OPA%DB_NAME, IVER, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4233, IUER, 'OPA_STSINQ', 'Failure to read '// &
     &         'superfile catalogue file and to learn whether superfile '// &
     &         'for experiment '//OPA%DB_NAME//' exists' )
           RETURN
      END IF
      IF ( IVER .GT. 0 ) THEN
           OPA%STS(OPA__SUP) = '+'
         ELSE
           OPA%STS(OPA__SUP) = '-'
      END IF
      IF ( OPA%STS(OPA__SUP) .EQ. '-' ) OPA%ACT(OPA__SUP) = '?'
!
! --- 4) Check baseline-dependent weight file
!
      CALL ERR_PASS ( IUER, IER )
      CALL WEIGHT_FILE_CHECK ( OPA%DB_NAME, IVER, OPA%BAS_WEIGHT_FILE, &
     &                         IPOS_WGT_BAS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4234, IUER, 'OPA_STSINQ', 'Error in attempt to '// &
     &         'find baseline weights in the file '// &
     &          OPA%BAS_WEIGHT_FILE(1:I_LEN(OPA%BAS_WEIGHT_FILE))// &
     &         'for experiment '//OPA%DB_NAME//' exists' )
           RETURN
      END IF
      IF ( IPOS_WGT_BAS .GT. 0 ) THEN
           OPA%STS(OPA__BAW) = '+'
         ELSE
           OPA%STS(OPA__BAW) = '-'
      END IF
      IF ( OPA%STS(OPA__BAW) .EQ. '-' ) OPA%ACT(OPA__BAW) = '?'
!
! --- 5) Check site-dependent weight file
!
      CALL ERR_PASS ( IUER, IER )
      CALL WEIGHT_FILE_CHECK ( OPA%DB_NAME, IVER, OPA%SIT_WEIGHT_FILE, &
     &                         IPOS_WGT_SIT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4235, IUER, 'OPA_STSINQ', 'Error in attempt to '// &
     &         'find site weights in the file '// &
     &          OPA%SIT_WEIGHT_FILE(1:I_LEN(OPA%SIT_WEIGHT_FILE))// &
     &         'for experiment '//OPA%DB_NAME//' exists' )
           RETURN
      END IF
      IF ( IPOS_WGT_SIT .GT. 0 ) THEN
           OPA%STS(OPA__STW) = '+'
         ELSE
           OPA%STS(OPA__STW) = '-'
      END IF
      IF ( OPA%STS(OPA__STW) .EQ. '-' ) OPA%ACT(OPA__STW) = '?'
!
! --- 6) Check stations and sources: whether all stations and sources are in
! --- the mapping file?
!
      CALL ERR_PASS ( IUER, IER )
      CALL STA_SOU_CHECK ( OPA%DB_NAME, OPA%GLO_SRC_FILE, OPA%GLO_STA_FILE, &
     &                     M_SOU, L_SOU, CLIS_SOU, M_STA, L_STA, CLIS_STA, &
     &                     DURATION, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4236, IUER, 'OPA_STSINQ', 'Failure to read '// &
     &         'superfile catalogue file and to learn whether superfile '// &
     &         'for experiment '//OPA%DB_NAME//' exists' )
           RETURN
      END IF
!
      IF ( L_SOU .GT. 0 ) THEN
           CALL LIST_TO_LINE ( L_SOU,  CLIS_SOU, ', ', OUT )
           CALL ERR_LOG ( 4237, IUER, 'OPA_STSINQ', 'Sources "'// &
     &          OUT(1:I_LEN(OUT))//'" which were observed in experiment '// &
     &          OPA%DB_NAME//' were not found in the source substitution '// &
     &          'file '//OPA%GLO_SRC_FILE(1:I_LEN(OPA%GLO_SRC_FILE))// &
     &          ' Please add them there' )
           RETURN
      END IF
!
      IF ( L_STA .GT. 0 ) THEN
           CALL LIST_TO_LINE ( L_STA, CLIS_STA, ', ', OUT )
           CALL ERR_LOG ( 4238, IUER, 'OPA_STSINQ', 'Stations "'// &
     &          OUT(1:I_LEN(OUT))//'" which participated in experiment '// &
     &          OPA%DB_NAME//' were not found in the station substitution '// &
     &          'file '//OPA%GLO_STA_FILE(1:I_LEN(OPA%GLO_STA_FILE))// &
     &          ' Please add them there' )
           RETURN
      END IF
!
      IF ( DURATION  .LT.  OPA%MIN_DURATION   .OR. &
     &     DURATION  .GT.  OPA%MAX_DURATION        ) THEN
!
! -------- Nominal session duration turned out to be out of range
!
           CALL CLRCH ( OUT )
           WRITE ( UNIT=OUT, FMT='(F12.1)' ) DURATION
           CALL CHASHL ( OUT )
           CALL ERR_LOG ( 4239, IUER, 'OPA_STSINQ', 'Nominal diratoin of '// &
     &         'session '//OPA%DB_NAME//' '//OUT(1:I_LEN(OUT))//' seconds '// &
     &         'is out of range specificed in the configuration file '// &
     &          OPA%CONFIG_FILE(1:I_LEN(OPA%CONFIG_FILE)) )
           RETURN
      END IF
!
! --- 7) Check EOPB file. Whether the database has contributed
! --- to computation of EOP derived from 24 hour session?
!
      IF ( OPA%EOPB_FILE(1:9) .NE. '/dev/null' ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL EOPS_CHECK ( OPA%DB_NAME, OPA%EOPB_FILE, OPA%STS(OPA__EOS), &
     &                       IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4240, IUER, 'OPA_STSINQ', 'Failure to check '// &
     &              'whether '//OPA%DB_NAME//' is in EOPB file '// &
     &               OPA%EOPB_FILE )
                RETURN
           END IF
      END IF
!
      IF ( OPA%STS(OPA__EOS) .EQ. 'N' ) THEN
           OPA%ACT(OPA__EOS) = 'N'
         ELSE IF ( OPA%STS(OPA__EOS) .EQ. '-' ) THEN
           OPA%ACT(OPA__EOS) = '?'
      END IF
!
      IF ( OPA%EOPT_FILE(1:9) .NE. '/dev/null' ) THEN
!
! -------- 8) Check EOPM file. Whether the database has contributed
! -------- to computation of EOPM derived from Intensive session?
!
           CALL ERR_PASS ( IUER, IER )
           CALL EOPS_CHECK ( OPA%DB_NAME, OPA%EOPT_FILE, OPA%STS(OPA__EOM), &
     &                       IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4241, IUER, 'OPA_STSINQ', 'Failure to check '// &
     &              'whether '//OPA%DB_NAME//' is in EOPM file '// &
     &               OPA%EOPT_FILE )
                RETURN
           END IF
      END IF
!
      IF ( OPA%STS(OPA__EOM) .EQ. 'N' ) THEN
           OPA%ACT(OPA__EOM) = 'N'
         ELSE IF ( OPA%STS(OPA__EOM) .EQ. '-' ) THEN
           OPA%ACT(OPA__EOM) = '?'
      END IF
!
! --- 9) Check SNR analysis files.
!
      IP = LINDEX ( OPC_FILE, '/' )
      IF ( IP .LE. 0 ) IP = I_LEN(OPC_FILE)
!
      CALL ERR_PASS ( IUER, IER )
      CALL SNRANAL_CHECK ( OPA%SESS_CODE, OPC_FILE(1:IP), OPA%STS(OPA__SNR), &
     &                     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4242, IUER, 'OPA_STSINQ', 'Failure to check '// &
     &         'whether SNR analysis for  '//OPA%DB_NAME//' has already '// &
     &         'been done' )
           RETURN
      END IF
!
      IF ( OPA%STS(OPA__SNR) .EQ. 'N' ) THEN
           OPA%ACT(OPA__SNR) = 'N'
         ELSE IF ( OPA%STS(OPA__SNR) .EQ. '-' ) THEN
           OPA%ACT(OPA__SNR) = '?'
      END IF
!
! --- 10) Check whether listings of standalone solutions in both spool and
! --- Sinex formats have been generated in stored?
!
      CALL ERR_PASS ( IUER, IER )
      CALL STANDALONE_CHECK ( OPC_FILE, OPA%STS(OPA__STN), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4243, IUER, 'OPA_STSINQ', 'Failure to check '// &
     &         'whether '//OPA%DB_NAME//' was submitted to the IVS Data '// &
     &         'Center' )
           RETURN
      END IF
!
      OPA%STS(OPA__SNX) = '?'
      IF ( OPA%STS(OPA__STN) .EQ. 'N' ) THEN
           OPA%ACT(OPA__STN) = 'N'
           OPA%STS(OPA__SNX) = 'N'
         ELSE IF ( OPA%STS(OPA__STN) .EQ. '-' ) THEN
           OPA%ACT(OPA__STN) = '?'
           OPA%STS(OPA__SNX) = 'N'
      END IF
!
! --- Write OPC file on disk once more
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRITE_OPC ( OPC_FILE, OPA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4244, IUER, 'OPA_STSINQ', 'Error in attempt '// &
     &         'to write the current status of experiment '//OPA%DB_NAME// &
     &         ' in file '//OPC_FILE )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  OPA_STSINQ  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SUPCAT_CHECK ( DB_NAME, IVER, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SUPCAT_CHECK searches database DB_NAME in superfile. If    *
! *   returns version number of the database/superfile. It doesn't find  *
! *   database, it returns 0, if it finds more than one database it      *
! *   returns version number of the last database.                       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  DB_NAME ( CHARACTER ) -- Database name. It may contain leading      *
! *                           dollar character, but may not.             *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    IVER ( INTEGER*4 ) -- Version number of the database/superilfe.   *
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
! *  ### 16-AUG-2000  SUPCAT_CHECK  v1.0 (c) L. Petrov  16-AUG-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'precm.i'
      CHARACTER  DB_NAME*(*)
      INTEGER*4  IVER, IUER
      CHARACTER  SUPCAT_FINAM*128, DBN*9, STR*32
      INTEGER*4  LUN, IO, J1, IV
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_UNIT
!
      IVER = 0
      CALL CLRCH ( DBN )
      IF ( DB_NAME(1:1) .EQ. '$' ) THEN
           DBN = DB_NAME(2:)
        ELSE
           DBN = DB_NAME
      END IF
!
! --- Check whether experiment is in the superfile list
!
!@      CALL CLRCH  ( SUPCAT_FINAM )
!@      CALL GETENVAR ( 'SUPCAT_FILE', SUPCAT_FINAM )
!@      IF ( ILEN(SUPCAT_FINAM) .EQ. 0 ) THEN
!@           SUPCAT_FINAM = PRE_SAV_DIR(1:PRE_SV_LEN)//SUPCAT_FILE
!@      END IF
!
! --- Open superfile catalgoue file
!
      LUN = GET_UNIT ()
      OPEN ( UNIT=LUN, FILE=SUPCAT_FINAM, STATUS='OLD', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 5171, IUER, 'SUPCAT_CHECK', 'Error '// &
     &          STR(1:I_LEN(STR))//' in attempt to open superfile catalogue '// &
     &          'file '//SUPCAT_FINAM )
           RETURN
      END IF
!
! --- Read superfile catalogue file
!
      DO 410 J1=1,1024*1024
         READ ( LUN, FMT='(A10,I3)', IOSTAT=IO ) STR(1:10), IV
         IF ( IO .EQ. -1 ) THEN
              GOTO 810
            ELSE IF ( IO .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IO, STR )
              WRITE ( 6, * ) ' line ',J1
              CALL ERR_LOG ( 5172, IUER, 'SUPCAT_CHECK', 'Error '// &
     &            STR(1:I_LEN(STR))//' in reading superfile catalogue '// &
     &          'file '//SUPCAT_FINAM  )
              RETURN
         END IF
         IF ( STR(1:9) .EQ. DBN ) IVER = IV
 410  CONTINUE
 810  CONTINUE
      CLOSE ( UNIT = LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SUPCAT_CHECK  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE WEIGHT_FILE_CHECK ( DB_NAME, IVER, WGHT_FILE, IPOS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine WEIGHT_FILE_CHECK  scans weight file WGHT_FILE and tries   *
! *   to find there weights for VLBI experiemrn with database name       *
! *   DB_NAME, version number IVER. If it finds it returns position of   *
! *   the first line with weights. If it doesn't find it returns 0.      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   DB_NAME ( CHARACTER ) -- Database name. It may contain leading     *
! *                            dollar character, but may not.            *
! *      IVER ( INTEGER*4 ) -- Version number of the database/superilfe. *
! * WGHT_FILE ( CHARACTER ) -- Weight file name.                         *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *      IPOS ( INTEGER*4 ) -- Position of the weight for the database   *
! *                            DB_NAME version IVER.                     *
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
! * ### 16-AUG-2000 WEIGHT_FILE_CHECK v1.0 (c) L. Petrov 16-AUG-2000 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'precm.i'
      CHARACTER  DB_NAME*(*)
      INTEGER*4  IVER, IUER
      CHARACTER  WGHT_FILE*128, DBN*9, STR*32
      INTEGER*4  LUN, IO, J1, IV, IPOS
      INTEGER*4, EXTERNAL :: I_LEN, GET_UNIT
!
      IPOS = 0
!
! --- Remove leading dollar sign in database name if it is there
!
      CALL CLRCH ( DBN )
      IF ( DB_NAME(1:1) .EQ. '$' ) THEN
           DBN = DB_NAME(2:)
        ELSE
           DBN = DB_NAME
      END IF
!
! --- Open weight file
!
      LUN = GET_UNIT ()
      OPEN ( UNIT=LUN, FILE=WGHT_FILE, STATUS='OLD', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 5211, IUER, 'WEIGHT_FILE_CHECK', 'Error '// &
     &          STR(1:I_LEN(STR))//' in attempt to open weight file '// &
     &          WGHT_FILE )
           RETURN
      END IF
!
! --- Read weight file
!
      DO 410 J1=1,1024*1024
         READ ( LUN, FMT='(A)', IOSTAT=IO ) STR
         IF ( IO .EQ. -1 ) THEN
              GOTO 810
            ELSE IF ( IO .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IO, STR )
              WRITE ( 6, * ) ' line ',J1
              CALL ERR_LOG ( 5212, IUER, 'WEIGHT_FILE_CHECK', 'Error '// &
     &             STR(1:I_LEN(STR))//' in attempt to read weight file '// &
     &             WGHT_FILE )
              RETURN
         END IF
!
! ------ Decode version number
!
         CALL CHIN ( STR(10:13), IV )
         IF ( STR(1:9) .EQ. DBN  .AND. IVER .EQ. IV ) THEN
              IPOS = J1
              GOTO 810
         END IF
 410  CONTINUE
 810  CONTINUE
      CLOSE ( UNIT = LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  WEIGHT_FILE_CHECK  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE STA_SOU_CHECK ( DB_NAME, GLO_SRC_FILE, GLO_STA_FILE, &
     &                           M_SOU, L_SOU, CLIS_SOU, &
     &                           M_STA, L_STA, CLIS_STA, DURATION, IUER )
! ************************************************************************
! *                                                                      *
! *   Procedure STA_SOU_CHECK  reads the first record of the database    *
! *   and checks whether all sources and stations which have been        *
! *   observed. Then it reads station and source mapping files.          *
! *   The procedure retudns lists of stations and sources which have     *
! *   been observed but were not found in global stations and source     *
! *   substition files. STA_SOU_CHECK returns nominal duration of        *
! *   session.                                                           *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      DB_NAME ( CHARACTER ) -- Database name. It may contain leading  *
! *                               dollar character, but may not.         *
! * GLO_SRC_FILE ( CHARACTER ) -- Source substitution file in Solve      *
! *                               source mapping format.                 *
! * GLO_STA_FILE ( CHARACTER ) -- Station substitution file in Solve     *
! *                               station mapping format.                *
! *        M_SOU ( INTEGER*4 ) -- Maximal number of sources in the       *
! *                               output list of source names.           *
! *        M_STA ( INTEGER*4 ) -- Maximal number of stations in the      *
! *                               output list of station names.          *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *        L_SOU ( INTEGER*4 ) -- Number of sources which were observed  *
! *                               in the session but were not found in   *
! *                               source substitution mapping file.      *
! *        L_STA ( INTEGER*4 ) -- Number of stations which were observed *
! *                               in the session but were not found in   *
! *                               source substitution mapping file.      *
! *     DURATION ( REAL*8    ) -- Nominal duration of the session in     *
! *                               seconds: time tag of the last          *
! *                               observation in the database minus time *
! *                               time tag of the first observation.     *
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
! *  ### 16-AUG-2000  STA_SOU_CHECK  v1.0 (c) L. Petrov 16-OCT-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INTEGER*4  M_SOU, M_STA, L_SOU, L_STA, IUER
      CHARACTER  DB_NAME*(*), GLO_SRC_FILE*(*), GLO_STA_FILE*(*), &
     &           CLIS_SOU(M_SOU)*8, CLIS_STA(M_STA)*8
      REAL*8     DURATION
      CHARACTER  STR*80, STR2*80, DOLLAR_DB_NAME*10, DATE_B*19, DATE_E*19
      INTEGER*4  MG_SOU, MG_STA
      PARAMETER  ( MG_SOU = MAX_SRC, MG_STA = MAX_STA )
      CHARACTER  GLIS_SOU(MG_SOU)*8, GLIS_STA(MG_STA)*8
      CHARACTER  DLIS_SOU(MG_SOU)*8, DLIS_STA(MG_STA)*8
      REAL*8     SEC_E, SEC_B
      LOGICAL*4  LEX
      INTEGER*4  LG_SOU, LG_STA, LD_SOU, LD_STA, IO, LUN, INTERVAL(5,2), &
     &           MJD_E, MJD_B, J1, J2, IER
      INTEGER*2  KERR_I2, I2
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: I_LEN, GET_UNIT
!
      L_SOU  = 0
      L_STA  = 0
      LG_SOU = 0
      LG_STA = 0
!
! --- Learn whether the source mapping file exists
!
      INQUIRE ( FILE=GLO_SRC_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5181, IUER, 'STA_SOU_CHECK', 'Source coordinates '// &
     &         'file '//GLO_SRC_FILE(1:I_LEN(GLO_SRC_FILE))//' was not found' )
           RETURN
      END IF
!
! --- Learn whether the station mapping file exists
!
      INQUIRE ( FILE=GLO_STA_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5182, IUER, 'STA_STA_CHECK', 'Statiuon coordinates '// &
     &         'file '//GLO_STA_FILE(1:I_LEN(GLO_STA_FILE))//' was not found' )
           RETURN
      END IF
!
! --- Open source coordinates file
!
      LUN = GET_UNIT ()
      OPEN ( UNIT=LUN, FILE=GLO_SRC_FILE, STATUS='OLD', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 5183, IUER, 'STA_SOU_CHECK', 'Error '// &
     &          STR(1:I_LEN(STR))//' in attempt to open source coordinates '// &
     &          'file '//GLO_SRC_FILE  )
           RETURN
      END IF
!
! --- Read source coordinates file
!
      DO 410 J1=1,1024*1024
         READ ( LUN, FMT='(A)', IOSTAT=IO ) STR
         IF ( IO .EQ. -1 ) THEN
              GOTO 810
            ELSE IF ( IO .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IO, STR )
              WRITE ( 6, * ) ' line ',J1
              CALL ERR_LOG ( 5184, IUER, 'STA_SOU_CHECK', 'Error '// &
     &             STR(1:I_LEN(STR))//' in reading source coordinates '// &
     &            'file '//GLO_SRC_FILE )
              RETURN
         END IF
         IF ( STR(1:1) .NE. ' ' ) GOTO 410
         LG_SOU = LG_SOU + 1
         GLIS_SOU(LG_SOU) = STR(5:12)
 410  CONTINUE
 810  CONTINUE
      CLOSE ( UNIT = LUN )
!
! --- Open station coordinates file
!
      LUN = GET_UNIT ()
      OPEN ( UNIT=LUN, FILE=GLO_STA_FILE, STATUS='OLD', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 5185, IUER, 'STA_SOU_CHECK', 'Error '// &
     &          STR(1:I_LEN(STR))//' in attempt to open station coordinates '// &
     &          'file '//GLO_STA_FILE  )
           RETURN
      END IF
!
! --- Read source coordinates file
!
      DO 420 J2=1,1024*1024
         READ ( LUN, FMT='(A)', IOSTAT=IO ) STR
         IF ( IO .EQ. -1 ) THEN
              GOTO 820
            ELSE IF ( IO .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IO, STR )
              WRITE ( 6, * ) ' line ',J2
              CALL ERR_LOG ( 5186, IUER, 'STA_SOU_CHECK', 'Error '// &
     &             STR(1:I_LEN(STR))//' in reading station coordinates '// &
     &            'file '//GLO_STA_FILE )
              RETURN
         END IF
         IF ( STR(1:1) .NE. ' ' ) GOTO 420
         LG_STA = LG_STA + 1
         GLIS_STA(LG_STA) = STR(5:12)
 420  CONTINUE
 820  CONTINUE
      CLOSE ( UNIT = LUN )
!
! --- Opening database for the X- band
!
      CALL CLRCH ( DOLLAR_DB_NAME )
      DOLLAR_DB_NAME = DB_NAME
      IF ( DOLLAR_DB_NAME(1:1) .NE. '$' ) DOLLAR_DB_NAME = '$'//DB_NAME
!@      CALL KAI ( INT2(1), INT2(0), INT2(0), INT2(1), DOLLAR_DB_NAME, INT2(0), &
!@     &          'same      ', STR, I2, STR, KERR_I2 )
!@      IF ( KERR_I2 .NE. INT2(0) ) THEN
!@           CALL CLRCH ( STR2 )
!@           CALL INCH  ( INT4(KERR_I2), STR2 )
!@           CALL ERR_LOG ( 5187, IUER, 'STA_SOU_CHECK', 'Error KAI for '// &
!@     &         'database '//DOLLAR_DB_NAME//' KERR='//STR2 )
!@           RETURN
!@      ENDIF
!@!
!@! --- Reading the preface records of the database
!@!
!@      CALL MVREC ( INT2(1), INT2(1), INT2(1), KERR_I2 )
!@      IF ( KERR_I2 .NE. 0 ) THEN
!@           CALL CLRCH ( STR2 )
!@           CALL INCH  ( INT4(KERR_I2), STR2 )
!@           CALL ERR_LOG ( 5188, IUER, 'STA_SOU_CHECK', 'Error in reading '// &
!@     &         'the first record of the for database '//DOLLAR_DB_NAME )
!@           RETURN
!@      ENDIF
!@!
!@! --- Getting information common to all observations
!@!
!@      CALL DBH_GETI4 ( '# STARS ', LD_SOU,   1,      1,     1, -3 )
!@      CALL DBH_GETCH ( 'STRNAMES', DLIS_SOU, LD_SOU, 1,        -3 )
!@      CALL DBH_GETI4 ( '# SITES ', LD_STA,   1,      1,     1, -3 )
!@      CALL DBH_GETCH ( 'SITNAMES', DLIS_STA, LD_STA, 1,        -3 )
!@      CALL DBH_GETI4 ( 'INTERVAL', INTERVAL, 5,      2,     1, -3 )
!@      CALL FINIS ( INT2(0) )
!
! --- Subtract session source from the global source list and put
! --- remnant to the L_SOU/CLIS_SOU list
!
      CALL ERR_PASS ( IUER, IER )
      CALL SUB_CLIST ( 0, LD_SOU, DLIS_SOU, LG_SOU, GLIS_SOU, M_SOU, &
     &                 L_SOU, CLIS_SOU, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5189, IUER, 'STA_SOU_CHECK', 'Error in operation '// &
     &         'source lists subtraction' )
           RETURN
      END IF
!
! --- Subtract session station list from the global source and put
! --- remnant to the L_STA/CLIS_STA list
!
      CALL ERR_PASS ( IUER, IER )
      CALL SUB_CLIST ( 0, LD_STA, DLIS_STA, LG_STA, GLIS_STA, M_STA, &
     &                 L_STA, CLIS_STA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5190, IUER, 'STA_SOU_CHECK', 'Error in operation '// &
     &         'station lists subtraction' )
           RETURN
      END IF
!
! --- Transformation the first date of the interval to MJD, SEC
!
      WRITE ( UNIT=DATE_B, FMT='(I4,".",I2,".",I2,"-",I2,":",I2,":00")' ) &
     &        INTERVAL(1,1), INTERVAL(2,1), INTERVAL(3,1), INTERVAL(4,1), &
     &        INTERVAL(5,1)
      IF ( INTERVAL(1,1) .LT. 70 ) THEN
           DATE_B(1:2) = '19'
         ELSE
           DATE_B(1:2) = '20'
      END IF
      CALL BLANK_TO_ZERO ( DATE_B )
      CALL ERR_PASS      ( IUER, IER )
      CALL DATE_TO_TIME  ( DATE_B, MJD_B, SEC_B, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5191, IUER, 'STA_SOU_CHECK', 'Error in first '// &
     &         'date of the interval decoding: '//DATE_B )
           RETURN
      END IF
!
! --- Transformation the last date of the interval to MJD, SEC
!
      WRITE ( UNIT=DATE_E, FMT='(I4,".",I2,".",I2,"-",I2,":",I2,":00")' ) &
     &        INTERVAL(1,2), INTERVAL(2,2), INTERVAL(3,2), INTERVAL(4,2), &
     &        INTERVAL(5,2)
      IF ( INTERVAL(1,2) .LT. 70 ) THEN
           DATE_E(1:2) = '19'
         ELSE
           DATE_E(1:2) = '20'
      END IF
      CALL BLANK_TO_ZERO ( DATE_E )
      CALL DATE_TO_TIME  ( DATE_E, MJD_E, SEC_E, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5192, IUER, 'STA_SOU_CHECK', 'Error in last '// &
     &         'date of the interval decoding: '//DATE_E )
           RETURN
      END IF
!
      DURATION = (MJD_E - MJD_B)*86400.0 + (SEC_E - SEC_B)
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  STA_SOU_CHECK  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   GLOARC_CHECK ( DB_NAME, DB_VER, ARC_FILE, IUER )
! ************************************************************************
! *                                                                      *
! *   Logical function  GLOARC_CHECK checks the file ARC_FILE which      *
! *   contains names of the superfiles used for global solution.         *
! *   If it finds there database name DB_NAME with version IVER it       *
! *   returns .TRUE., otherwise it returns .FALSE.                       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  DB_NAME ( CHARACTER ) -- Database name. It may contain leading      *
! *                           dollar character, but may not.             *
! *   DB_VER ( INTEGER*4 ) -- Database version number.                   *
! * ARC_FILE ( CHARACTER ) -- Arc file which is to be checked.           *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <GLOARC_CHECK> ( LOGICAL*4 ) -- result of the check:                 *
! *                                 .TRUE. -- Database was found in      *
! *                                 global arc-list.                     *
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
! *  ### 15-SEP-2000  GLOARC_CHECK v1.0 (c)  L. Petrov  15-SEP-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  DB_VER, IUER
      CHARACTER  DB_NAME*(*), ARC_FILE*(*)
      LOGICAL*4  GLOARC_CHECK
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 8192, MIND = 32 )
      CHARACTER  BUF(MBUF)*32, DBN*10, REG*3, DBN_RD*10
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      INTEGER*4  LIND, IND(2,MIND), NBUF, J1, IER, DBN_RD_VER
!
      GLOARC_CHECK = .FALSE.
!
! --- Make DBN -- database name without dollar sign
!
      CALL CLRCH ( DBN )
      IF ( DB_NAME(1:1) .EQ. '$' ) THEN
           DBN = DB_NAME(2:)
         ELSE
           DBN = DB_NAME
      END IF
!
! --- Read file with global arc-list to the internal buffere BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( ARC_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4241, IUER, 'GLOARC_CHECK', 'Error in attempt to '// &
     &         'read global arc-file' )
           RETURN
      END IF
!
! --- Scan this buffer from the lst line to the first
!
      DO 410 J1=NBUF,1,-1
         IF ( BUF(J1)(1:1) .EQ. '*' ) GOTO 410
!
! ------ Extract words
!
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, IER )
         IF ( LIND .LT. 2 ) GOTO 410
!
         CALL CLRCH ( DBN_RD )
!
! ------ Extract version number
!
         DBN_RD = BUF(J1)(IND(1,1):IND(2,1))
         READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I4)' ) DBN_RD_VER
!
! ------ Compare database name and database version number
!
         IF ( DBN_RD(1:1) .EQ. '$' ) THEN
              IF ( DBN_RD .EQ. '$'//DBN  .AND.  DB_VER .EQ. DBN_RD_VER ) THEN
                   GLOARC_CHECK = .TRUE.
                   GOTO 810
              END IF
            ELSE
              IF ( DBN_RD .EQ. DBN  .AND.  DB_VER .EQ. DBN_RD_VER ) THEN
                   GLOARC_CHECK = .TRUE.
                   GOTO 810
              END IF
         END IF
 410  CONTINUE
 810  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GLOARC_CHECK  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EOPS_CHECK ( DB_NAME, EOPB_FILE, STATUS_STR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  EOPS_CHECK checks whether session DB_NAME is in the EOP   *
! *   file in B-format. It return the status line STATUS_STR which can   *
! *   be "-" -- not found, "N" -- found and this session is marked as    *
! *   participated in global solution, "+" -- found and this session     *
! *   is marked as NOT participated in global solution.                  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    DB_NAME ( CHARACTER ) -- Database name. It may contain leading    *
! *                             dollar character, but may not.           *
! *  EOPB_FILE ( CHARACTER ) -- EOP file in IERS B-format which will be  *
! *                             checked.                                 *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * STATUS_STR ( CHARACTER ) -- Status of the estimation of EOP using    *
! *                             this database:                           *
! *                             "-" -- not found,                        *
! *                             "N" -- found and this session is marked  *
! *                                    as participated in global         *
! *                                    solution,                         *
! *                             "+" -- found and this session is marked  *
! *                                    as NOT participated in global     *
! *                                    solution.                         *
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
! *  ### 19-SEP-2000   EOPS_CHECK  v1.0 (c)  L. Petrov  19-SEP-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'getpar.i'
      CHARACTER  DB_NAME*(*), EOPB_FILE*(*), STATUS_STR*(*)
      INTEGER*4  IUER
      TYPE ( EOB__CHAR ) ::  EOB(M_SES)
      LOGICAL*4  LEX
      CHARACTER  DBN*9
      INTEGER*4  J1, N_SES, IER
      INTEGER*4, EXTERNAL :: ILEN
!
      CALL CLRCH ( STATUS_STR )
      STATUS_STR = 'N'
!
      INQUIRE ( FILE=EOPB_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      CALL CLRCH ( DBN )
      IF ( DB_NAME(1:1) .EQ. '$' ) THEN
           DBN = DB_NAME(2:)
        ELSE
           DBN = DB_NAME
      END IF
!
! --- Read EOB file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( %REF(EOPB_FILE), M_SES, EOB, N_SES, IER, &
     &                %VAL(LEN(EOPB_FILE)), %VAL(SIZEOF(EOB(1))) )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4281, IUER, 'EOPS_CHECK', 'Error in attempt '// &
     &                   'to read EOP file in EOB-format '//EOPB_FILE )
           RETURN
      END IF
!
! --- Scan the file
!
      DO 410 J1=N_SES,1,-1
         IF ( ILEN(EOB(J1)%DBNAME) .LE.  0  ) GOTO 410  ! bypass empty line
         IF ( EOB(J1)%FLAG(1:1)    .EQ. '#' ) GOTO 410  ! or comment lines
!
         IF ( EOB(J1)%DBNAME(2:10) .EQ. DBN ) THEN
              IF ( EOB(J1)%FLAG .EQ. '@' ) THEN
                   STATUS_STR = 'N'
                ELSE
                   STATUS_STR = '+'
              END IF
              CALL ERR_LOG ( 0, IUER )
              RETURN
         END IF
 410  CONTINUE
!
      STATUS_STR = '-'
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  EOPS_CHECK  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE STANDALONE_CHECK ( OPC_FILE, STATUS_STR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  STANDALONE_CHECK  checks whether the listings in both     *
! *   Sinex and  spool formates were gerneated and stored at the local   *
! *   analysis centers.                                                  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   OPC_FILE ( CHARACTER ) -- The name of the file which keeps         *
! *                             OPA statsus processing this particular   *
! *                             session.                                 *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * STATUS_STR ( CHARACTER ) -- Status of the estimation of EOP using    *
! *                             this database:                           *
! *                             "-" -- not found,                        *
! *                             "+" -- both listings were found.         *
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
! * ### 06-JUN-2002  STANDALONE_CHECK v1.0 (c) L. Petrov 06-JUN-2002 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  OPC_FILE*(*), STATUS_STR*(*)
      INTEGER*4  IUER
      LOGICAL*4  LEX
      CHARACTER  SPL_FIL*256, SNX_FIL*256
      INTEGER*4  IP, IS, STATB(12)
      INTEGER*4, EXTERNAL :: ILEN, FOR_STAT
!
      STATUS_STR = '-'  ! For now...
!
! --- Build the full filename of
!
      IP = MAX ( 1, ILEN(OPC_FILE) - 4 )
      SPL_FIL = OPC_FILE(1:IP)//'.spl'
!
! --- Check whether this file exists
!
      INQUIRE ( FILE=SPL_FIL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- CHeck the length of this file
!
      IS = FOR_STAT ( SPL_FIL, STATB )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      ENDIF
      IF ( STATB(8) .EQ. 0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      ENDIF
!
! --- Build the full filename of Sinex listing
!
      SNX_FIL = OPC_FILE(1:IP)//'.snx'
!
! --- Check whether this file exists
!
      INQUIRE ( FILE=SNX_FIL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           STATUS_STR = '-'
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Check the length of this file
!
      IS = FOR_STAT ( SNX_FIL, STATB )
      IF ( IS .NE. 0 ) THEN
           STATUS_STR = '-'
           CALL ERR_LOG ( 0, IUER )
           RETURN
      ENDIF
      IF ( STATB(8) .EQ. 0 ) THEN
           STATUS_STR = '-'
           CALL ERR_LOG ( 0, IUER )
           RETURN
      ENDIF
!
! --- Both spool file and Sinex file exist and their lenght is not zero.
! --- It means somebody has already run OPA and created both spool files and
! --- Sinex listings
!
      STATUS_STR = '+'
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  STANDALONE_CHECK  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SNRANAL_CHECK ( SESS_CODE, SESS_DIR, STATUS_STR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SNRANAL_CHECK  checks whether SNR analysis for this       *
! *   database has already been done.                                    *
! *                                                                      *
! *  ### 10-JAN-2001  SNRANAL_CHECK  v1.0 (c) L. Petrov 16-JAN-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  SESS_CODE*(*), SESS_DIR*(*), STATUS_STR*(*)
      CHARACTER  SNRRES_FIL*128
      LOGICAL*4  LEX
      INTEGER*4  IUER
      INTEGER*4  STATB(12), IS
      INTEGER*4, EXTERNAL :: FOR_STAT, I_LEN
!
! --- Build the full filename of
!
      IF ( SESS_DIR(I_LEN(SESS_DIR):) .EQ. '/' ) THEN
           SNRRES_FIL = SESS_DIR//SESS_CODE(1:I_LEN(SESS_CODE))//'.snranal'
         ELSE
           SNRRES_FIL = SESS_DIR//'/'//SESS_CODE(1:I_LEN(SESS_CODE))//'.snranal'
      END IF
!
! --- Check whether this file exists
!
      INQUIRE ( FILE=SNRRES_FIL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           STATUS_STR = '-'
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- CHeck the length of this file
!
      IS = FOR_STAT ( SNRRES_FIL, STATB )
      IF ( IS .NE. 0 ) THEN
           STATUS_STR = '-'
           CALL ERR_LOG ( 0, IUER )
           RETURN
      ENDIF
      IF ( STATB(8) .EQ. 0 ) THEN
           STATUS_STR = '-'
           CALL ERR_LOG ( 0, IUER )
           RETURN
      ENDIF
!
! --- File exists and its lenght is not zero. It means somebody has already
! --- run SNRANAL
!
      STATUS_STR = '+'
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SNRANAL_CHECK  #!#
