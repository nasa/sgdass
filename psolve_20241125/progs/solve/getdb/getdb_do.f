      SUBROUTINE GETDB_DO ( VCAT, GVH, VTD, DBNAME, ENV_NAME, FL_BATCH, &
     &                      FL_COMP_THEO, FL_SOU_USE_DB_IGNORE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GETDB
! *                                                                      *
! *  ### 30-NOV-2005   GETDB_DO   v1.11 (c)  L. Petrov  25-MAR-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'astro_constants.i'
      INCLUDE   'socom.i'
      INCLUDE   'precm.i'
      INCLUDE   'prfil.i'
      INCLUDE   'gvh.i'
      INCLUDE   'vcat.i'
      INCLUDE   'vtd.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc2.i'
      INCLUDE   'glbc3.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom_plus.i'
      INTEGER*4  IUER
      CHARACTER  ENV_NAME*(*)
      TYPE     ( GVH__STRU  ) :: GVH
      TYPE     ( VCAT__TYPE ) :: VCAT
      TYPE     ( VTD__TYPE  ) :: VTD
      TYPE     ( CAL__TYPE  ) :: CAL(M__CAL)
!
      INTEGER*4    M_FIL, MBUF
      PARAMETER  ( M_FIL =  32 ) 
      PARAMETER  ( MBUF  = 8192 ) 
      CHARACTER  FILENV*128, FILIN(M_FIL)*128, STR*80, DBNAME*10
      CHARACTER  GVF_DB_DIR*128, GVF_ENV_DIR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, I, &
     &           L_FIL, NBUF, MJD_BEG, MJD_END, IB, IE, IV, IL, &
     &           ISL_MAX, ISEG_SL, LUN, L_LIN, ID, IND_REP, IER
      LOGICAL*4  FL_BATCH, FL_SOU_USE_DB_IGNORE, FL_FOUND, LEX
      REAL*8     TAI_BEG, TAI_END, CLOOF_ACM(M_ACM), CLODR_ACM(M_ACM)
      INTEGER*4  L_STA, L_SOU, L_ACM, L_CAL, EXP_VERSION
      CHARACTER  EXP_NAME*16, STAT_ACM(M_ACM)*8, VTD_CONF_SES_SAVE*128
      CHARACTER  C_STA(MAX_ARC_STA)*8, C_SOU(MAX_ARC_SRC)*8, BUF(MBUF)*128
      CHARACTER  RW_BAS_NAM(2,MAX_ARC_BSL)*8, WORK_DIR*128, HIST_FIL*128, TITLE*128
      REAL*8     RW_BAS_DEL(0:SLV__MAX_SOLTYP-1,MAX_ARC_BSL), &
     &           RW_BAS_RAT(0:SLV__MAX_SOLTYP-1,MAX_ARC_BSL)
      INTEGER*4  N_MET(MAX_STA), IND_MET(2,MAX_OBS)
      REAL*8     ATM_PRES(MAX_SCA,MAX_STA), AIR_TEMP(MAX_SCA,MAX_STA), &
     &           TIM_MET(MAX_SCA,MAX_STA)
      REAL*8       JUL_YEAR__TO__SEC
      INTEGER*8  K81, K82
      PARAMETER  ( JUL_YEAR__TO__SEC = 365.25D0*86400.0D0 ) ! Julian year
      LOGICAL*4  FL_PARAM, FL_COMP_THEO 
      INTEGER*8, EXTERNAL :: GET_MEMRSS
      LOGICAL*2, EXTERNAL :: KBIT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, LTM_DIF, GET_UNIT, &
     &                       OMP_GET_NUM_THREADS, OMP_GET_MAX_THREADS
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
      CALL ERR_PASS ( IUER, IER )
      CALL PSOLVE_VERSION_CHECK ( IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8810, IUER, 'GETDB_DO', 'Trap of the '// &
     &         'internal control: version mismatch has been detected.'// &
     &         ' This may happened when some package is compiled against '// &
     &         'one version and linked againt another. Please recompile' )
           RETURN 
      END IF
!
      VTD_CONF_SES_SAVE = VTD_CONF_SES
      CALL USE_GLBFIL   ( 'OR' )
      CALL USE_GLBFIL_3 ( 'R'  )
      CALL USE_GLBFIL_4 ( 'RC'  )
!
      IF ( FL_BATCH ) THEN
           IF ( INDEX ( ENV_NAME, '/' ) > 0 ) THEN
!
! ------------- Full path envelop file name
!
                FILENV = ENV_NAME
                ID = LINDEX ( ENV_NAME, '/' )
!
! ------------- Search for the envelop directory name index
!
                IND_REP = 0
                DO 410 J1=1,VCAT%NREPS
                   IF ( ENV_NAME(1:ID-1) == VCAT%GVF_ENV_DIR(J1) ) THEN
                        IND_REP = J1
                        VCAT_REPO = VCAT%GVF_REP_NAME(J1)
                        GOTO 810
                   END IF
 410            CONTINUE 
 810            CONTINUE 
                IF ( IND_REP < 1 ) THEN
                     CALL ERR_LOG ( 8811, IUER, 'GETDB_DO', 'Trap of the '// &
     &                   'internal control: envelop directory of the file '// &
     &                    TRIM(ENV_NAME)//' is not defined in the '// &
     &                   ' VCAT control file '//VCAT%CONF_FILE )
                     RETURN 
               END IF
              ELSE 
                IND_REP = 1
                FILENV  = TRIM(VCAT%GVF_ENV_DIR(IND_REP))//'/'//ENV_NAME
           END IF
           GVF_ENV_DIR = VCAT%GVF_ENV_DIR(IND_REP)
           GVF_DB_DIR  = VCAT%GVF_DB_DIR(IND_REP)
         ELSE
           CALL CLRCH ( VTD_CONF_SES )
           CALL ERR_PASS ( IUER, IER )
!
! -------- Check for the repository name
!
           IF ( ILEN(VCAT_REPO) == 0 ) THEN
                IND_REP = 1
              ELSE
                IND_REP = LTM_DIF ( 0, VCAT%NREPS, VCAT%GVF_REP_NAME, VCAT_REPO )
                IF ( IND_REP < 1 ) THEN
                     CALL ERR_LOG ( 8812, IUER, 'GETDB_DO', 'VCAT repository name '// &
     &                    TRIM(VCAT_REPO)//' defined in the environment variable '// &
     &                   'VCAT_REPO is not defined in VCAT control file '// &
     &                    VCAT%CONF_FILE )
                     RETURN 
                END IF
           END IF
           GVF_ENV_DIR = VCAT%GVF_ENV_DIR(IND_REP)
           GVF_DB_DIR  = VCAT%GVF_DB_DIR(IND_REP)
           CALL GETDB_SELECT ( GVF_ENV_DIR, FILENV, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8813, IUER, 'GETDB_DO', 'Error in an '// &
     &              'attempt to read database envelope file '//FILENV )
                RETURN 
           END IF
           IF ( ILEN (FILENV) == 0 ) THEN
                CALL ERR_LOG ( 0, IUER )
                RETURN 
           END IF
           CALL GETENVAR ( 'VTD_CONF_SES', VTD_CONF_SES )
           CALL GETENVAR ( 'PSOLVE_CHECK_VCAT', STR )
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:1) == 'Y' ) THEN
                WRITE ( 6, * ) 'GETDB_DO-1: env var VTD_CONF_SES= '//TRIM(VTD_CONF_SES)
                CALL HIT_CONT ( 'Hit any key to continue', 0 ) 
           END IF
!
           IF ( ILEN(VTD_CONF_SES) == 0 ) THEN
                VTD_CONF_SES = VCAT%VTD_CONF_SES_FILE
           END IF
           IF ( STR(1:1) == 'Y' ) THEN
                WRITE ( 6, * ) 'GETDB_DO-2: VTD_CONF_SES= '//TRIM(VTD_CONF_SES)
                CALL HIT_CONT ( 'Hit any key to continue', 0 ) 
           END IF
!
           IF ( ILEN(VTD_CONF_SES) == 0 ) THEN
                IF ( ILEN(VCAT%CONF_FILE) > 0 ) THEN
                     CALL ERR_LOG ( 8815, IUER, 'GETDB_DO', 'Cannot find '// &
     &                   'VTD configuration file in the VCAT configuration '// &
     &                   'attempt to read database envelope file '//FILENV )
                     RETURN 
                   ELSE
                     CALL ERR_LOG ( 8816, IUER, 'GETDB_DO', 'Cannot find '// &
     &                   'VTD configuration file in the VCAT configuration '// &
     &                   'file '//TRIM(VCAT%CONF_FILE)//' please check '// &
     &                   'its format and check for the keyword VTD_CONF_FILE:' )
                     RETURN 
                END IF
           END IF
      END IF
      IB = LINDEX ( FILENV, '/' ) + 1
      IE =  INDEX ( FILENV(IB:), '_' ) + IB-2
      IF ( IE .LE. IB ) IE = ILEN(FILENV)
      IV  = LINDEX ( FILENV, '_v' )
      EXP_NAME = FILENV(IB:IB+9)
      CALL CHIN ( FILENV(IV+2:IV+4), EXP_VERSION )
      IF ( .NOT. FL_BATCH ) THEN
           VTD_CONF_SES_SAVE = VTD_CONF_SES
           WRITE ( 6, 110 ) EXP_NAME, EXP_VERSION, TRIM(VTD_CONF_SES)
 110       FORMAT ( 'Loading database ', A, ' ver ', I3, ' VTD: ', A )
         ELSE
           CONTINUE 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_READ_DB ( FILENV, GVF_DB_DIR, GVH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8817, IUER, 'GETDB_DO', 'Error in an attempt to '// &
     &         'read database '//FILENV )
           RETURN 
      END IF
!
      ISEG_SL = 0
      DO 420 J2=1,GVH%NSEG
         IL = ILEN(GVH%FILENAME(J2))
         IF ( IL .GE. 12 ) THEN
              IF ( GVH%FILENAME(J2)(IL-11:IL-9) == 'sl1' ) THEN
                   ISEG_SL = J2
              END IF
         END IF
 420  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_INIT ( VTD,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8818, IUER, 'GETDB_DO', 'Error in an attempt to '// &
     &         'initialize VTD' )
           RETURN 
      END IF
! 
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_CONF ( VTD_CONF_SES, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8819, IUER, 'GETDB_DO', 'Error in an attempt to '// &
     &         'parse VTD configuration file '//VTD_CONF_SES )
           RETURN 
      END IF
!
      CALL GETENVAR ( 'VTD_TEST', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, VTD%CONF%TEST(1) )
      END IF
!
      IF ( .NOT. FL_BATCH ) THEN
           FL_VTD_SES = .TRUE.
           VTD_CONF_SES = VTD_CONF_SES_SAVE
           CALL USE_GLBFIL_4 ( 'OWC'  )
      END IF      
!
      CALL ERR_PASS ( IUER, IER )
      CALL GETDB_FILL_SOCOM ( GVH, FL_PARAM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8820, IUER, 'GETDB_DO', 'Error in an attempt to '// &
     &         'fill SOCOM with values from database files' )
           RETURN 
      END IF
      IF ( .NOT. FL_BATCH ) THEN
!
! -------- Set default elevation cutoff angle 
!
           CALL GETENVAR ( 'PSOLVE_ELMIN', STR )
           IF ( ILEN(STR) > 0 ) THEN
                READ ( UNIT=STR, FMT=*, IOSTAT=IER ) ELMIN
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8821, IUER, 'GETDB_DO', 'Cannot parse '// &
     &                   'environment variable PSOLVE_ELMIN: '//TRIM(STR)// &
     &                   ' a float number (elevation angle in degrees) was expected' )
                     RETURN 
                END IF
                ELMIN = DEG__TO__RAD*ELMIN
                DO I=1, MAX_ARC_STA
                   ELVCUT(I) = ELMIN
                END DO
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GETDB_FILL_PARFIL ( GVH, L_STA, C_STA, L_SOU, C_SOU, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8822, IUER, 'GETDB_DO', 'Error in an attempt to '// &
     &         'fill NAMFIL with values from database files' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GETDB_FILL_NAMFIL ( GVH, EXP_NAME, EXP_VERSION, MJD_BEG, TAI_BEG, &
     &                         MJD_END, TAI_END, L_ACM, STAT_ACM, CLOOF_ACM, &
     &                         CLODR_ACM, L_CAL, CAL, RW_BAS_NAM, RW_BAS_DEL, &
     &                         RW_BAS_RAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8823, IUER, 'GETDB_DO', 'Error in an attempt to '// &
     &         'fill NAMFIL with values from database files' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GETDB_METEO_CORRECT ( GVH, N_MET, IND_MET, TIM_MET, ATM_PRES, &
     &                           AIR_TEMP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8824, IUER, 'GETDB_DO', 'Error in an attempt to '// &
     &         'correct missing meteorological data' )
           RETURN 
      END IF
!
#ifdef DEBUG
      WRITE ( 6, 110 ) EXP_NAME, EXP_VERSION ; CALL FLUSH ( 6 )
#endif
      CALL ERR_PASS ( IUER, IER )
      CALL GETDB_FILL_OBORG ( GVH, L_CAL, CAL, FL_COMP_THEO, &
     &                        FL_SOU_USE_DB_IGNORE, &
     &                        N_MET, IND_MET, ATM_PRES, AIR_TEMP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8825, IUER, 'GETDB_DO', 'Error in an attempt to '// &
     &         'fill OBORG with values from database files' )
           RETURN 
      END IF
!
      IF ( FL_COMP_THEO ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL  COMP_THEO ( GVH, VTD, L_STA, C_STA, L_SOU, C_SOU, MJD_BEG, &
     &                       TAI_BEG, MJD_END, TAI_END, L_ACM, STAT_ACM, &
     &                       CLOOF_ACM, CLODR_ACM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8826, IUER, 'GETDB_DO', 'Error in an '// &
     &              'attempt to computed theoretical path delay using VTD '// &
     &              'with configuration file '// &
     &               VTD_CONF_SES(1:I_LEN(VTD_CONF_SES)) )
                RETURN 
           END IF
         ELSE 
           IF ( VTD%SESS_NAME == DBNAME .AND. VTD%STATUS == VTD__LOAD ) THEN
!
! ------------- The data for this experiment has been loaded. Nothing to do
!
                CONTINUE 
              ELSE 
                CALL ERR_PASS ( IUER, IER )
                CALL VTD_LOAD ( VTD, L_STA, C_STA, L_SOU, C_SOU, MJD_BEG, &
     &                          TAI_BEG, MJD_END, TAI_END, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8827, IUER, 'GETDB_DO', 'Error in '// &
     &                   'an attempt to load files with auxiliary '// &
     &                   'information need for GETDB using VTD with '// &
     &                   'configuration file '//VTD_CONF_SES )
                     RETURN 
                END IF
           END IF
           VTD%SESS_NAME = DBNAME_CH
!
           DO 450 J5=1,L_STA
              FL_FOUND = .FALSE.
              DO 460 J6=1,VTD%L_STA
                 IF ( VTD%STA(J6)%IVS_NAME == C_STA(J5) ) THEN
                      FL_FOUND = .TRUE.
                      VSITEC(1,J5) = VTD%STA(J6)%COO_TRS(1,1) 
                      VSITEC(2,J5) = VTD%STA(J6)%COO_TRS(2,1)
                      VSITEC(3,J5) = VTD%STA(J6)%COO_TRS(3,1)
!    
                      VSITEV(1,J5) = VTD%STA(J6)%VEL_TRS(1)*JUL_YEAR__TO__SEC 
                      VSITEV(2,J5) = VTD%STA(J6)%VEL_TRS(2)*JUL_YEAR__TO__SEC
                      VSITEV(3,J5) = VTD%STA(J6)%VEL_TRS(3)*JUL_YEAR__TO__SEC
                      CALL CLRCH (                         MONUMENTS_CHR(J5)      )
                      CALL INCH  ( VTD%STA(J6)%CDP_NUMBER, MONUMENTS_CHR(J5)(1:4) )
                  END IF
 460          CONTINUE 
 450       CONTINUE 
      END IF
!
      DO 470 J7=1,L_SOU
         DO 480 J8=1,VTD%L_SOU
            IF ( VTD%SOU(J8)%IVS_NAME == C_SOU(J7) ) THEN
                 JNAME(J7) = VTD%SOU(J8)%J2000_NAME
            END IF
 480     CONTINUE 
 470  CONTINUE 
!
      CALCV = -101.0D0
!
! --- Get the name of the history file
!
      CALL GETENVAR ( 'PSOLVE_WORK_DIR', WORK_DIR )
      IF ( ILEN(WORK_DIR) == 0 ) WORK_DIR = SOLVE_WORK_DIR
      HIST_FIL = TRIM(WORK_DIR)//'/'//'HIST'//PRE_LETRS
!
! --- Check whether it exists. Remove, if exists
!
      INQUIRE ( FILE=HIST_FIL, EXIST=LEX ) 
      IF ( LEX ) THEN
           CALL UNLINK ( TRIM(HIST_FIL)//CHAR(0) )
      END IF
      IF ( ISEG_SL > 0 ) THEN
           IF ( GVH%TEXT(ISEG_SL)%NTIT > 0 ) THEN
!
! ------------- Extract solution history from the text file and store it in 
! ------------- the histry file in order to pick it up during section update
!
                LUN=GET_UNIT()
                OPEN ( UNIT=LUN, FILE=HIST_FIL, STATUS='NEW', IOSTAT=IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8828, IUER, 'GETDB_DO', 'Error in an '// &
     &                  'attempt to open history file '//HIST_FIL )
                     RETURN 
                END IF
                WRITE ( LUN, '(A,I6,A)' ) '@section_length: ', GVH%TEXT(ISEG_SL)%NTIT, ' chapters'
                DO 490 J9=1,GVH%TEXT(ISEG_SL)%NTIT
                   CALL ERR_PASS ( IUER, IER )
                   CALL GVH_GTEXT_CHP ( GVH, ISEG_SL, J9, MBUF, L_LIN, TITLE, BUF, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8829, IUER, 'GETDB_DO', 'Error in an '// &
     &                      'reading text secion of the the input file' )
                        RETURN 
                   END IF
                   IF ( ILEN(TITLE) > LEN('characters ') ) THEN
                        IF ( TITLE(1:LEN('characters')) == 'characters' ) THEN
                             TITLE = TITLE(12:)
                        END IF
                   END IF
                   WRITE ( LUN, '(A,1X,I2,2X,I6,1X,A,A)' ) &
     &                     '  @@chapter: ', J9, L_LIN, ' records, ', TITLE(1:I_LEN(TITLE))
                   DO 4100 J10=1,L_LIN
                      WRITE ( LUN, '(A)' ) TRIM(BUF(J10))
 4100              CONTINUE 
 490            CONTINUE 
                CLOSE ( UNIT=LUN )
           END IF
      END IF
!
      CALL CLRCH ( ENV_FINAM )
      ENV_FINAM = FILENV 
!
      CALL ERR_PASS ( IUER, IER )
      CALL GETDB_PARAM ( VTD, GVH, FL_PARAM, MJD_BEG, TAI_BEG, MJD_END, &
     &                   TAI_END, L_SOU, C_SOU, L_STA, C_STA, FL_BATCH, &
     &                   FL_SOU_USE_DB_IGNORE, RW_BAS_NAM, RW_BAS_DEL, &
     &                   RW_BAS_RAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8830, IUER, 'GETDB_DO', 'Error in an attempt to '// &
     &         'set up parameterization' ) 
           RETURN 
      END IF
      SOCOM_PLUS_FIRST = 0
      CALL SOCOM_EXT()
!
      IF ( FL_BATCH ) THEN
#ifdef DEBUG
           WRITE ( 6, 120 ) EXP_NAME, EXP_VERSION
 120       FORMAT ( 'Loaded  database ', A, '  version ', I3 )
#endif
           CONTINUE 
         ELSE 
           CALL USE_GLBFIL_4 ( 'OR'  )
           FL_VTD_SES = .FALSE.
           VTD_CONF_SES = VTD_CONF_SES_SAVE
           VTD_ADR = 0
           CALL USE_GLBFIL_4 ( 'WC'  )
           CALL USE_COMMON ( 'OR' ) 
           INIT_INTERACTIVE = 1
           CALL USE_COMMON ( 'WC' ) 
           CALL HIT_CONT ( 'The database is loaded. Hit any key to continue', 0 ) 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GETDB_DO  !#!#
