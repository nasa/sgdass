#include <mk5_preprocessor_directives.inc>
      SUBROUTINE GOUTPT ( SOLTYP, RSTOUT, MINOUT, BASOUT, FWDOUT, SCNOUT, &
     &                    TBLOUT, POSELL, POSEPOCH, POSNUM, MODOUTFLG, RESFILE, &
     &                    KMIN_SIG, KOUTNRM, KZERONRM )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GOUTPT PROGRAM SPECIFICATION
!
! 1.1 Parse OUTPUT section of the control file.
!
! 1.2 REFERENCES:
!
! 2.  GOUTPT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'fast.i'
      INCLUDE 'precm.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables:
!
      CHARACTER SOLTYP*(*), RSTOUT*(*), MINOUT*(*), BASOUT*(*), &
     &          SCNOUT*(*), TBLOUT*(*), POSELL*(*), RESFILE*(*)
      INTEGER*2 FWDOUT, POSNUM
      REAL*8    POSEPOCH
      LOGICAL*2 MODOUTFLG, KMIN_SIG, KOUTNRM, KZERONRM
      ADDRESS__TYPE  DIR_DESC
!
      ADDRESS__TYPE, EXTERNAL :: FUNC_OPENDIR
      INTEGER*4,     EXTERNAL :: CLOSEDIR
!
! BASOUT - Baseline ouput flag
! FWDOUT - Flag for output for forward solution
! MINOUT - Flag for minimum output to spool file
! RSTOUT - True if spool file is to be reset
! SCNOUT - Screen output flag
! TBLOUT - Station-table flag
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ctrlfl
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      CHARACTER    STRING*256, TOKEN*256, STR*64, STR1*20, OUT*1024
      CHARACTER    FNAME*128, ERRSTR*160
      INTEGER*4    FILDES, JSIZE, IS, IP
      INTEGER*2    IL, IERR, YEAR_DIG2
      INTEGER*2    LENGTH, IDUM, ERROR, ITOK(40), YEAR, MO, DAY, IDAY0
      EQUIVALENCE ( TOKEN, ITOK )
      LOGICAL*2    KRST, KMIN, KBAS, KFWD, KSCN, KCVR, KCOR, KSTATBL, &
     &             KELL, KMOD, KRES, KMSG, KNRM, KSNX, KEMU, KNRD, KCHI, &
     &             KION, KSEG_OUTPUT, &
     &             KMAPPED_EOP_OUTPUT, KFAT, KSPL
      LOGICAL*4    KCVR_YES, KCOR_YES, CORRELATIONS_PARSER, KLOPT       
!
!  EXTERNAL FUNCTIONS
!
      INTEGER*2 CFREAD,TRIMLEN,DECIMALTOINT
      LOGICAL*2 CFEOF, GTCVR
!
! INITIALIZATION
!
      DATA KRST /.FALSE./, KMIN /.FALSE./, KBAS /.FALSE./, KFWD /.FALSE./
      DATA KSCN /.FALSE./, KCVR /.FALSE./, KCOR /.FALSE./, KSTATBL/.FALSE./
      DATA KMOD /.FALSE./, KRES /.FALSE./, KMSG /.FALSE./, KNRM /.FALSE./
      DATA KSNX /.FALSE./, KELL /.FALSE./, KFAT /.FALSE./, KSPL /.FALSE./
      DATA KEMU /.FALSE./, KCHI /.FALSE./, KNRD /.FALSE./, KION /.FALSE./
!
      LOGICAL*4  FL_OVERWRITE, LEX
      INTEGER*4  IMIN4, IHOU4, IOS, J1, IUER
      REAL*8     SEC8
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: I_LEN, LTM_DIF, TIME
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   KDB 951109   Fix error in parsing normal_matrix keyword (won't accept
!                zero token).  Also check for illegal normal_matrix token.
!   PET 971006   Added a keyword SEG_OUTPUT
!   jwr 971101   Added keywork MAPPED_EOP_OUTPUT
!   pet 980206   Added a qualifier MAPPED_EOP_TIME to the keyword
!                MAPPED_EOP_OUTPUT.
!   KDB 980223   New keyword: SINEX, which controls sinex output.
!   pet 980703   Added a trap to block using FORWARD=value when SAVING_RATE is
!                not 1 or the same value
!   pet 981221   Fixed a bug SEG_OUTPUT default was ignored if no SEG_OUTPUT
!                line was specifed in the control file
!   kdb 990206   Y2K changes.
!   pet 990419   Added a trap of incompatibility fast_mode = f__b1b3d and
!                covariance yes.
!   pet 990930   Added supoport of a new keyword CORRELATIONS. Improved
!                comments and error messages.
!   pet 2000.03.31  Added a trap of incompatibility of SOLUTION INDEPENDENT
!                   and COVARIANCE YES options (covariances are generated only
!                   in the backward run of global solution)
!   pet 2000.04.27  Added initialization of KELL
!   pet 2000.05.01  Added support of the keyword APRIORI_ZENDEL
!   pet 2000.06.02  Added NORMAL_MATRIX NO
!   pet 2002.03.27  Removed arguments LSINEX, LLSNXDIR. Replaced the code
!                   which parsed SINEX keyword
!   pet 2002.03.29  Substantially changed syntax of SINEX keyword.
!   pet 2002.03.28  Added support of three new keywords: CRES_EMULATION,
!                   NRD_TABLE, CHI_SQUARE_TABLE
!   pet 2003.08.12  Made changes realted to using CRES_STYLE variable 
!                   instead of TITLE_ANON, TITLE_PRE98, CRES_PRE98
!   pet 2003.11.13  Replaced ias2b with READ
!   pet 2004.03.12  Added support of  a new qualifier in SINEX keyword: &
!                   ALLOW_OVERWRITE. If ALLOW_OVERWRITE has a value NO, then &
!                   goutput checks whether the output listing already exists. &
!                   If yes, it detects an error.
!   pet 2005.03.18  Added support of a new keyword LISTING_OPTIONS
!   pet 2005.07.21  Added support of a new qualifier of the keyword &
!                   LISTING_OPTIONS SEG_STYLE
!   pet 2021.06.01  Added support of a new option for LISTING_OPTIONS SRC_STAT: &
!                   POST2021
!
! 5.  GOUTPT PROGRAM STRUCTURE
!
      KMAPPED_EOP_OUTPUT = .FALSE.
      KCVR_YES           = .FALSE.
      KCOR_YES           = .FALSE.
      TBLOUT      =  'N'
      POSELL      =  'N'
      POSEPOCH    =  0.D0
      POSNUM      =  0
      MODOUTFLG   = .TRUE.
      KMIN_SIG    = .FALSE.
      KOUTNRM     = .FALSE.
      KZERONRM    = .FALSE.
      KSEG_OUTPUT = .FALSE.
      KLOPT       = .FALSE.
      FL_OVERWRITE  = .FALSE.
!
      CALL USE_GLBFIL_4 ( 'OR' )
!
      FL_SINEX_MAKE = .FALSE.
      FL_SINEX_GLO  = .FALSE.
      FL_SINEX_LOC  = .FALSE.
      FL_SINEX_SEG  = .FALSE.
      FL_SINEX_EST  = .FALSE.
      FL_SINEX_COV  = .FALSE.
      FL_SINEX_CNS  = .FALSE.
      FL_SINEX_DCM  = .FALSE.
      CALL CLRCH ( SINEX_OUTFILE  )
      CALL CLRCH ( SINEX_INCLUDE  )
      CALL CLRCH ( SINEX_EXCLUDE  )
      CALL CLRCH ( SINEX_ACKFIL   )
      CALL CLRCH ( SINEX_COMFIL   )
      CALL CLRCH ( SINEX_REALNAME )
      CALL CLRCH ( SINEX_VERS     )
!
! --- Additional initialization
!
      SEG_OUTPUT        =  SEG_OUTPUT__DEF
      RESFILE           = 'NONE'
      RESOUTFILE        = 'NONE'
      MAPPED_EOP_OUTPUT = .FALSE.
      COROUT_FORM       =  CRL__UND
      COR_FLAG          = .FALSE.
      COR_GG_FLAG       = .FALSE.
      COR_GL_FLAG       = .FALSE.
      COR_LL_FLAG       = .FALSE.
      COR_CL_FLAG       = .FALSE.
      APRIORI_ZENDEL    = APRIORI_ZENDEL__DEF
      FL_NRD_TABLE      = .TRUE.
      FL_CHI_TABLE      = .FALSE.
      SRC_LISTING_STYLE = SRC_PRE2004_SPOOL__FMT
      SEG_LISTING_STYLE = SEG_LISTING_STYLE__DEF
!
      CALL CLRCH ( COR_GG_INCFIL )
      CALL CLRCH ( COR_GG_EXCFIL )
      CALL CLRCH ( COR_GL_INCFIL )
      CALL CLRCH ( COR_GL_EXCFIL )
      CALL CLRCH ( COR_LL_INCFIL )
      CALL CLRCH ( COR_LL_EXCFIL )
      CALL CLRCH ( COR_CL_INCFIL )
      CALL CLRCH ( COR_CL_EXCFIL )
      CALL CLRCH ( COR_CL_INCSES )
      CALL CLRCH ( COR_CL_EXCSES )
      CALL CLRCH ( GIM_INFO_DIR  )
      CALL CLRCH ( GIM_DTEC_DIR  )
      CALL CLRCH ( GIM_ADDW_DIR  )
      CALL CLRCH ( GIM_DEL_DIR   )
      CALL CLRCH ( GIM_NOI_DIR   )
      CALL CLRCH ( BCL_FIL       )
      CALL CLRCH ( BRK_FIL       )
      GIM_MODE = 0
      GIM_DEG  = 0
      GIM_SEED = 0
      GIM_VERB = 0
      GIM_TIM_STEP = 0.0
      GIM_SCALE    = 0.0
      GIM_COLLECT_INFO = .FALSE.
      GIM_EST          = .FALSE.
      GIM_WRI          = .FALSE.
      GIM_RGR          = .FALSE.
!
      CRES_STYLE  = CRES__CURRENT
!
! --- Read first line of OUTPUT section and loop over lines until
! --- we find something in column 1
!
      LENGTH=CFREAD(STRING)
      DO WHILE ( STRING(1:1).EQ.' ' .AND. .NOT. CFEOF(IDUM) )
         DO WHILE(TRIMLEN(STRING).GT.0)
!
! --------- Pull the next token from the input string
!
            CALL SPLITSTRING(STRING,TOKEN,STRING )
!
! --------- 'RESET' KEYWORD
!
            IF ( TOKEN .EQ. 'RESET' ) THEN
                 IF ( KRST ) CALL FERR ( INT2(6010), &
     &               'BATCH(goutpt) keyword RESET '//'used twice', INT2(0), &
     &                INT2(0) )
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'YES' ) THEN
                      RSTOUT='Y'
                    ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                      RSTOUT='N'
                    ELSE
                      CALL FERR ( INT2(6012), 'BATCH(goutpt) illegal reset '// &
     &                    'parameter '//TOKEN(1:16), INT2(0), INT2(0) )
                 ENDIF
                 KRST = .TRUE.
              ELSE IF ( TOKEN .EQ. 'MINIMUM' ) THEN
!
! ------------- 'MINIMUM' KEYWORD
!
                 IF ( KMIN ) CALL FERR ( INT2(6020), &
     &               'BATCH(goutpt) keyword '//'MINIMUM used twice', INT2(0), &
     &                INT2(0) )
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'YES' ) THEN
                      MINOUT = 'Y'
                   ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                      MINOUT = 'N'
                   ELSE
                      CALL FERR ( INT2(6022), &
     &                    'BATCH(goutpt) illegal MINIMUM '//'parameter '// &
     &                     TOKEN(1:16), INT2(0), INT2(0) )
                 ENDIF
                 KMIN = .TRUE.
              ELSE IF ( TOKEN .EQ. 'POS_ELLIPSES' ) THEN
!
! ------------- 'POS_ELLIPSES' KEYWORD
!
                 IF ( KELL ) CALL FERR ( INT2(6030), &
     &               'BATCH(goutpt) keyword '//'POS_ELLIPSES used twice', &
     &                INT2(0), INT2(0) )
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'YES' ) THEN
                      POSELL='Y'
                   ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                      POSELL='N'
                   ELSE
                      CALL FERR ( INT2(6032), &
     &                    'BATCH(goutpt) illegal MINIMUM '//'parameter '// &
     &                     TOKEN(1:16), INT2(0), INT2(0) )
                 ENDIF
                 KELL=.TRUE.
              ELSE IF ( TOKEN .EQ. 'BASELINES' ) THEN
!
! ------------- 'BASELINES' KEYWORD
!
                 IF ( KMIN ) CALL FERR ( INT2(6040), &
     &               'BATCH(goutpt) BASELINES '//'used twice', INT2(0), INT2(0) )
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'YES' ) THEN
                      BASOUT='Y'
                   ELSE IF(TOKEN.EQ.'NO') THEN
                      BASOUT='N'
                   ELSE
                      CALL FERR ( INT2(6042), &
     &                    'BATCH(goutpt) illegal baselines '//'parameter '// &
     &                     TOKEN(1:16), INT2(0), INT2(0) )
                 ENDIF
                 KBAS=.TRUE.
              ELSE IF ( TOKEN .EQ. 'FORWARD' ) THEN
!
! ------------- 'FORWARD' KEYWORD
!
                 IF ( KFWD ) CALL FERR ( INT2(6050), &
     &               'BATCH(goutpt) FORWARD '//'used twice', INT2(0), INT2(0) )
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 FWDOUT = DECIMALTOINT ( TOKEN, ERROR )
                 IF ( TOKEN .EQ. 'YES' ) THEN
                      FWDOUT=1
                    ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                      FWDOUT=0
                    ELSE IF ( TOKEN .EQ. 'LAST' ) THEN
                      FWDOUT=-1
                    ELSE IF ( FWDOUT.LT.0 .OR. ERROR.NE.0 ) THEN
                      CALL FERR ( INT2(6052), &
     &                    'BATCH(goutpt) illegal FORWARD '//'parameter '// &
     &                     TOKEN(1:16), INT2(0), INT2(0) )
                 ENDIF
!
                 IF ( FWDOUT .GT. 1           .AND. &
     &                SAVING_RATE .GT. 1      .AND. &
     &                FWDOUT .NE. SAVING_RATE       ) THEN
!
                      CALL CLRCH ( STR )
                      CALL INCH  ( SAVING_RATE, STR   )
                      CALL CLRCH ( STR1 )
                      CALL INCH  ( INT4(FWDOUT), STR1 )
                      CALL ERR_LOG ( 6411, IUER, 'GOUTPT', &
     &                    'Conflicting options: SAVING_RATE='//STR(1:I_LEN(STR))// &
     &                    ' and FORWARD='//STR1(1:I_LEN(STR1))// &
     &                    '. FORWARD=value is '// &
     &                    'allowed only if SAGING_RATE=1, or values of '// &
     &                    'SAVING_RATE and FORWARD are the same' )
                      CALL FERR ( INT2(6054), &
     &                    'BATCH(goutpt) Conflicting values of '// &
     &                    'the options SAVING_RATE and FORWARD', INT2(0), INT2(0) )
                      STOP 'BATCH(goutpt) Abnormal termination'
                 END IF
                 KFWD=.TRUE.
              ELSE IF ( TOKEN .EQ. 'SCREEN' ) THEN
!
! -------------- 'SCREEN' KEYWORD
!
                 IF ( KSCN ) CALL FERR ( INT2(6060), &
     &               'BATCH(goutpt) keyword SCREEN '//'used twice', INT2(0), &
     &                INT2(0) )
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'YES' ) THEN
                      SCNOUT='Y'
                   ELSE IF(TOKEN.EQ.'NO') THEN
                      SCNOUT='N'
                   ELSE
                      CALL FERR ( INT2(6062), &
     &                    'BATCH(goutpt) illegal screen '//'parameter '// &
     &                     TOKEN(1:16), INT2(0), INT2(0) )
                 ENDIF
                 KSCN=.TRUE.
               ELSE IF ( TOKEN .EQ. 'MAPPED_EOP_OUTPUT' ) THEN
!
! ------------- 'MAPPED_EOP_OUTPUT' KEYWORKD
!
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'YES'  .OR. TOKEN .EQ. 'yes' ) THEN
                      MAPPED_EOP_OUTPUT = .TRUE.
!
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
                      IF ( INDEX ( '0123456789', TOKEN(1:1) ) .EQ. 0 ) THEN
                           CALL ERR_LOG ( 6412, -2, 'GOUTPT', &
     &                         'Wrong qualifier for the keyword '// &
     &                         'MAPPED_EOP_OUTPUT: '//TOKEN(1:I_LEN(TOKEN))// &
     &                         ' -- Time of mapping epoch is in hours or in '// &
     &                         ' the format hh_mm_ss.fff was expected' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
!
! ------------------- Parsing time of the mapping eop epopch
!
                      IF ( INDEX ( TOKEN, '_' ) .EQ. 0 .AND. &
     &                     INDEX ( TOKEN, ':' ) .EQ. 0       ) THEN
!
! ------------------------ Time is specified in hours
!
                           READ ( UNIT=TOKEN, FMT='(F12.6)', IOSTAT=IOS) &
     &                            MAPPED_EOP_TIME
                           IF ( IOS .NE. 0 ) THEN
                                CALL ERR_LOG ( 6413, -2, 'GOUTPT', &
     &                              'Wrong format of MAPPED_EOP_TIME token: '// &
     &                               TOKEN )
                                STOP 'BATCH(goutpt)  abnormal termination'
                           END IF
                           MAPPED_EOP_TIME = MAPPED_EOP_TIME*3600.0D0 + 32.184D0
                         ELSE IF ( ( TOKEN(3:3) .EQ. '_' .AND. &
     &                               TOKEN(6:6) .EQ. '_'       ) .OR. &
     &                             ( TOKEN(3:3) .EQ. ':' .AND. &
     &                               TOKEN(6:6) .EQ. ':'       )      ) THEN
!
! ------------------------ Time specified in the format hh_mm_ss.fff
!
                           CALL CHIN ( TOKEN(1:2), IHOU4 )
                           CALL CHIN ( TOKEN(4:5), IMIN4 )
                           READ ( UNIT=TOKEN(7:), FMT='(F12.6)' ) SEC8
                           IF ( IHOU4 .LT. 0   .OR. IHOU4 .GE. 24   .OR. &
     &                          IMIN4 .LT. 0   .OR. IMIN4 .GE. 60   .OR. &
     &                          SEC8 .LT. 0.0 .OR.  SEC8 .GE. 60.0     ) THEN
                                WRITE ( 6, * ) ' sec8=', sec8, ' imin4=', imin4, &
     &                                 ' ihou4=', ihou4
                                CALL ERR_LOG ( 6414, -2, 'GOUTPT', &
     &                              'Wrong format of MAPPED_EOP_TIME token: '// &
     &                               TOKEN )
                                STOP 'BATCH(goutpt)  abnormal termination'
                           END IF
                           MAPPED_EOP_TIME = SEC8 + IMIN4*60 + IHOU4*3600 + &
     &                                       32.184D0
                         ELSE
                           CALL ERR_LOG ( 6415, -2, 'GOUTPT', &
     &                         'Wrong format of MAPPED_EOP_TIME token: '//TOKEN )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF ! index token
!
                      IF ( MAPPED_EOP_TIME .GT. 86400.0 ) THEN
                           WRITE ( 6, * ) ' mapped_eop_time = ',mapped_eop_time
                           CALL ERR_LOG ( 6416, -2, 'GOUTPT', &
     &                         'Wrong format of MAPPED_EOP_TIME token: time '// &
     &                         'exceeds 86400 seconds: '//TOKEN )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                   ELSE IF ( TOKEN(1:2) .EQ. 'NO' .OR. &
     &                       TOKEN(1:2) .EQ. 'no' ) THEN
                      MAPPED_EOP_OUTPUT = .FALSE.
                   ELSE
                      CALL ERR_LOG ( 6417, -2, 'GOUTPT', &
     &                    'Unsupported token after MAPPED_EOP_TIME: '// &
     &                     TOKEN(1:I_LEN(TOKEN))//' someting like NO or '// &
     &                    '"YES hh_mm_ss.ff" was expected.' )
                      STOP 'BATCH(goutpt)  abnormal termination'
                 ENDIF ! token
              ELSE IF ( TOKEN .EQ. 'MOD_FILES' ) THEN
!
! ------------- 'MOD_FILES' KEYWORD
!
                 IF ( KMOD ) CALL FERR ( INT2(6070), &
     &               'BATCH(goutpt) keyword '//'MOD_FILES used twice', INT2(0), &
     &                INT2(0) )
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'YES' ) THEN
                      MODOUTFLG = .TRUE.
                    ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                      MODOUTFLG = .FALSE.
                    ELSE
                      CALL FERR ( INT2(6072), &
     &                    'BATCH(goutpt) illegal mod_files '//'parameter '// &
     &                     TOKEN(1:16), INT2(0), INT2(0) )
                 ENDIF
                 KMOD = .TRUE.
              ELSE IF ( TOKEN .EQ. 'MINIMIZE_SIGMAS' ) THEN
!
! ------------- 'MINIMIZE_SIGMAS' KEYWORD
!
                IF ( KMSG ) CALL FERR ( INT2(6080), 'BATCH(goutpt) keyword '// &
     &              'MINIMIZE_SIGMAS used twice', INT2(0), INT2(0) )
                CALL SPLITSTRING ( STRING, TOKEN, STRING )
                IF ( TOKEN .EQ. 'YES' ) THEN
                     KMIN_SIG = .TRUE.
                  ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                     KMIN_SIG = .FALSE.
                  ELSE
                     CALL FERR ( INT2(6082), 'BATCH(goutpt) illegal '// &
     &                   'MINIMIZE_SIGMAS parameter '//TOKEN(1:16), INT2(0), &
     &                    INT2(0) )
                ENDIF
                KMSG = .TRUE.
              ELSE IF ( TOKEN .EQ. 'RESIDUALS' ) THEN
!
! ------------- 'RESIDUALS' KEYWORD
!
                IF ( KRES ) CALL FERR ( INT2(6090), 'BATCH(goutpt) keyword '// &
     &              'RESIDUALS used twice', INT2(0), INT2(0) )
                CALL SPLITSTRING ( STRING, TOKEN, STRING )
                IF ( TOKEN(1:3) == 'NO ' ) TOKEN(1:5) = 'NONE '
                RESFILE = TOKEN(1:TRIMLEN(TOKEN))
                IF ( RESFILE(1:4) == 'NONE' )  THEN
                     CONTINUE 
                  ELSE IF ( RESFILE(1:4) .EQ. 'FULL' )  THEN
                     CONTINUE 
                  ELSE IF ( RESFILE(1:4) .EQ. 'SPOOL' )  THEN
                     CONTINUE 
                  ELSE 
                     IL = TRIMLEN(RESFILE)
                     FNAME = RESFILE(1:IL)//'.bin'
                     CALL BIN_UNLINK ( FNAME, IERR )
                     JSIZE = 0
                     CALL BIN_CREATE8 ( FNAME, FILDES, INT8(JSIZE) )
                     IF ( FILDES .LT. 0 ) THEN
                          WRITE ( ERRSTR, &
     &                          '("BATCH(goutpt) Couldn''t create: ",A)') TRIM(FNAME)
                          CALL FERR ( INT2(6092), ERRSTR, INT2(0), INT2(0) )
                     ENDIF
!
                     CALL BIN_CLOSE ( FNAME, FILDES )
                     IERR = 0
                     FNAME = RESFILE(1:IL)//'.ndx'
                     CALL USE_RESNDX ( 'OIC', FNAME )
                 ENDIF
                 KRES=.TRUE.
              ELSE IF ( TOKEN .EQ. 'COVARIANCES' ) THEN
!
! ------------- 'COVARIANCES' KEYWORD
!
                 IF ( KCVR ) CALL FERR ( INT2(6100), &
     &               'BATCH(goutpt) keyword '//'COVARIANCES used twice', INT2(0), &
     &                INT2(0) )
!
! -------------- KCVR_YES is TRUE if COVARIANCE YES was specified
!
                 KCVR_YES = GTCVR ( TOKEN, STRING, ' ' )
                 IF ( KCVR_YES .AND. FAST_MODE .EQ. F__B1B3D ) THEN
                      CALL ERR_LOG ( 6418, -2, 'GOUTPT', &
     &                    'Conflicting options: keyword COVARINACE is not '// &
     &                    'supported in fast mode ' )
                      CALL FERR ( INT2(6104), &
     &                    'Conflicting values of the options '// &
     &                    'FAST_MODE and COVARIANCE', INT2(0), INT2(0) )
                      STOP 'BATCH(goutpt) Abnormal termination'
                 END IF
!
                 IF ( KCVR_YES .AND. SOLTYP .EQ. 'I' ) THEN
                      CALL ERR_LOG ( 6419, -2, 'GOUTPT', &
     &                    'Conflicting options: keyword COVARINACE is not '// &
     &                    'supported in independent mode: use either '// &
     &                    'CORRELATIONS or set global mode' )
                      CALL FERR ( INT2(6105), &
     &                    'Conflicting values of the options: '// &
     &                    'SOLUTION INDEPENDENT and COVARIANCE YES', INT2(0), &
     &                     INT2(0) )
                      STOP 'BATCH(goutpt) Abnormal termination'
                 END IF
!
                 IF ( KCVR_YES .AND. KCOR_YES ) THEN
                      CALL FERR ( INT2(6106), &
     &                    'BATCH(goutpt) keywords COVARIANCES '// &
     &                    'and CORRELATIONS cannot be used together: at '// &
     &                    'least one of them should have value NO', INT2(0), &
     &                     INT2(0) )
                 END IF
                 KCVR = .TRUE.
              ELSE IF ( TOKEN .EQ. 'CORRELATIONS' ) THEN
                 IF ( KCOR ) CALL FERR ( INT2(6110), &
     &               'BATCH(goutpt) keyword '//'CORRELATIONS used twice', &
     &                INT2(0), INT2(0) )
!
! -------------- Parse a value of keyword CORRELATIONS
!
                 IUER = -1
                 KCOR_YES = CORRELATIONS_PARSER ( STRING, IUER )
                 IF ( IUER .NE. 0 ) THEN
                      CALL ERR_LOG ( 6420, -2, 'GOUTPT', &
     &                    'Error during parsing value of keyword CORRELATIONS' )
                      CALL FERR ( INT2(6114), &
     &                    'Error during parsing value of '// &
     &                    'keyword CORRELATIONS', INT2(0), INT2(0) )
                      STOP 'BATCH(goutpt) Abnormal termination'
                 END IF
                 IF ( KCVR_YES .AND. KCOR_YES ) THEN
                      CALL FERR ( INT2(6116), &
     &                    'BATCH(goutpt) keywords COVARIANCES '// &
     &                    'and CORRELATIONS cannot be used together: at '// &
     &                    'least one of them should have value NO', INT2(0), &
     &                     INT2(0) )
                 END IF
                 KCOR = .TRUE.
              ELSE IF ( TOKEN .EQ. 'NORMAL_MATRIX' ) THEN
!
! ------------- 'NORMAL_MATRIX' KEYWORD
!
                 IF ( KNRM ) CALL FERR ( INT2(6120), &
     &               'BATCH(goutpt) keyword '//'NORMAL_MATRIX used twice', &
     &                INT2(0), INT2(0) )
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'YES' ) THEN
                      KOUTNRM = .TRUE.
                    ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                      CONTINUE
                    ELSE IF ( TOKEN .EQ. 'ZERO' ) THEN
                      KZERONRM = .TRUE.
                    ELSE
                      CALL FERR ( INT2(6122), 'BATCH(goutpt) illegal '// &
     &                    'NORMAL_MATRIX token: '//TOKEN(1:16), INT2(0), INT2(0) )
                 END IF
                 KNRM = .TRUE.
              ELSE IF ( TOKEN .EQ. 'STATION_TABLE' ) THEN
!
! ------------- 'STATION_TABLE' KEYWORD
!
                 IF ( KSTATBL ) CALL FERR ( INT2(6130), &
     &               'BATCH(goutpt) keyword '//'STATION_TABLE used twice', &
     &                INT2(0), INT2(0) )
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'YES' ) THEN
                      TBLOUT='Y'
                      IF ( STRING .NE. ' ' ) THEN
                           CALL SPLITSTRING ( STRING, TOKEN, STRING )
                           READ ( UNIT=TOKEN(1:6), FMT='(3I2)', IOSTAT=IOS ) &
     &                            YEAR_DIG2, MO, DAY  
                           IF ( IOS .NE. 0 ) THEN
                                CALL ERR_LOG ( 6421, -2, 'GOUTPT', &
     &                            'Wrong format of the qualifier of the '// &
     &                            'STATION_TABLE keyword: '//TOKEN )
                                STOP 'GMAP(BATCH) Abnormal termination'
                           END IF
                           CALL NEWCENTS ( YEAR_DIG2, YEAR )
                           POSEPOCH = YEAR + (IDAY0(YEAR,MO)+DAY)/365.D0
                           IF ( STRING .NE. ' ' ) THEN
                                CALL SPLITSTRING ( STRING, TOKEN, STRING )
                                POSNUM = DECIMALTOINT ( TOKEN, ERROR )
                           ENDIF
                      ENDIF
                   ELSE IF(TOKEN.EQ.'NO') THEN
                      TBLOUT='N'
                   ELSE
                      CALL FERR ( INT2(6132), 'BATCH(goutpt) illegal '// &
     &                    'STATION_TABLE parameter '//TOKEN(1:16), INT2(0), &
     &                     INT2(0) )
                 ENDIF ! token
                 KSTATBL=.TRUE.
              ELSE IF ( TOKEN .EQ. 'SINEX' ) THEN
!
! ------------- 'SINEX' KEYWORD
!
                 IF ( KSNX ) CALL FERR ( INT2(6140), 'BATCH(goutpt) SINEX '// &
     &               'used twice', INT2(0), INT2(0) )
                 IUER = -1
                 CALL SINEX_PARSER ( STRING, FL_SINEX_MAKE, FL_OVERWRITE, &
     &                FL_SINEX_GLO, FL_SINEX_LOC, FL_SINEX_SEG, FL_SINEX_EST,  &
     &                FL_SINEX_COV, FL_SINEX_CNS, FL_SINEX_DCM, SINEX_OUTFILE, &
     &                SINEX_INCLUDE, SINEX_EXCLUDE, SINEX_ACKFIL, &
     &                SINEX_COMFIL, SINEX_VERS, IUER )
                 IF ( IUER .NE. 0 ) THEN
                      CALL ERR_LOG ( 6422, -2, 'GOUTPT', &
     &                    'Error during parsing value of keyword SINEX' )
                      STOP 'BATCH(goutpt) Abnormal termination'
                 END IF
!
                 IF ( FL_SINEX_MAKE ) THEN
                      IP = LTM_DIF ( 1, N__SNX__VERS, SNX__VERS, SINEX_VERS )
                      IF ( IP .LE. 0 ) THEN
                           CALL LIST_TO_LINE ( N__SNX__VERS, SNX__VERS, ' ', &
     &                                         OUT )
                           CALL ERR_LOG ( 6423, -2, 'BOUTPUT', &
     &                         'Wrong value of the qualifer FORMAT_VERSION: '// &
     &                          SINEX_VERS//' only versions '//OUT(1:I_LEN(OUT))// &
     &                         ' are supported' )
                           STOP 'BATCH(goutpt) Abnormal termination'
                      END IF
                 END IF
                 KSNX=.TRUE.
              ELSE IF ( TOKEN .EQ. 'SEG_OUTPUT' ) THEN
!
! -------------- 'SEG_OUTPUT' KEYWORD
!
                 IF ( KSEG_OUTPUT ) CALL FERR ( INT2(6150), 'BATCH(goupt) '// &
     &               'Parameter SEG_OUTPUT used twice', INT2(0), INT2(0) )
                 CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'YES' ) THEN
                      SEG_OUTPUT = .TRUE.
                   ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                      SEG_OUTPUT = .FALSE.
                   ELSE
                      CALL FERR ( INT2(6152), &
     &                    'Illegal SEG_OUTPUT qualifier '//TOKEN(1:16)// &
     &                    ' YES or NO were expected', INT2(0), INT2(0) )
                      STOP 'BATCH(goutpt) Abnormal termination'
                 ENDIF
                 KSEG_OUTPUT = .TRUE.
              ELSE IF ( TOKEN .EQ. 'APRIORI_ZENDEL' ) THEN
                 IF ( KFAT ) CALL FERR ( INT2(6160), 'BATCH(goupt) '// &
     &               'Parameter APRIORI_ZENDEL used twice', INT2(0), INT2(0) )
                 CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'YES' ) THEN
                      APRIORI_ZENDEL = .TRUE.
                   ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                      APRIORI_ZENDEL = .FALSE.
                   ELSE
                      CALL FERR ( INT2(6162), &
     &                    'Illegal APRIORI_ZENDEL qualifier '//TOKEN(1:16)// &
     &                    ' YES or NO were expected', INT2(0), INT2(0) )
                 ENDIF
                 KFAT = .TRUE.
              ELSE IF ( TOKEN .EQ. 'CRES_EMULATION' ) THEN
                 IF ( KEMU ) CALL FERR ( INT2(6170), &
     &               'BATCH(goutpt) CRES_EMULATION '//'used twice', INT2(0), &
     &                INT2(0) )
!
! -------------- CRES_EMULATION keyword
!
                 CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                 IF ( TOKEN(1:2) .EQ. 'NO' ) THEN
                      CRES_STYLE = CRES__CURRENT
                    ELSE IF ( TOKEN(1:6) .EQ. '199804' ) THEN
                      CRES_STYLE = CRES__PRE98  
                    ELSE IF ( TOKEN(1:6) .EQ. '200308' ) THEN
                      CRES_STYLE = CRES__PRE03
                    ELSE
                      CALL ERR_LOG ( 6424, -2, 'GOUTPT', 'Wrong '// &
     &                    'CRES_EMULATION value: '//TOKEN(1:12)// &
     &                    ' -- only values NO or 199804 and 200308 are '// &
     &                    'acceptable' )
                      STOP 'BATCH(goutpt)  abnormal termination'
                 END IF
                 KEMU = .TRUE.
              ELSE IF ( TOKEN .EQ. 'LISTING_OPTIONS' ) THEN
                 IF ( KLOPT ) CALL FERR ( INT2(6176), &
     &               'BATCH(goutpt) LISTING_OPTIONS used twice', INT2(0), &
     &                INT2(0) )
                 CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                 DO 410 J1=1,2
                    IF ( TOKEN .EQ. 'SRC_STAT' ) THEN
                         CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                         IF ( TOKEN == 'PRE2004' ) THEN
                              SRC_LISTING_STYLE = SRC_PRE2004_SPOOL__FMT
                            ELSE IF ( TOKEN == 'SHORT' ) THEN
                              SRC_LISTING_STYLE = SRC_SHORT_SPOOL__FMT
                            ELSE IF ( TOKEN == 'LONG'  ) THEN
                              SRC_LISTING_STYLE = SRC_LONG_SPOOL__FMT
                            ELSE IF ( TOKEN == 'POST2021'  ) THEN
                              SRC_LISTING_STYLE = SRC_POST2021_SPOOL__FMT
                            ELSE IF ( TOKEN == 'POST2024'  ) THEN
                              SRC_LISTING_STYLE = SRC_POST2024_SPOOL__FMT
                            ELSE 
                              CALL ERR_LOG ( 6425, -2, 'GOUTPT', 'Wrong '// &
     &                            'value of SRC_STAT qualifier of the '// &
     &                            'LISTING_OPTIONS keyword '//TOKEN(1:12)// &
     &                            ' -- only values PRE2004, SHORT, LONG, '// &
     &                            'POST2021, POST2024 are acceptable' )
                              STOP 'BATCH(goutpt)  abnormal termination'
                         END IF
                      ELSE IF ( TOKEN .EQ. 'SEG_STYLE' ) THEN
                         CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                         IF ( TOKEN == 'PRE2005' ) THEN
                              SEG_LISTING_STYLE = SEG_PRE2005_SPOOL__FMT 
                            ELSE IF ( TOKEN == 'POST2005' ) THEN
                              SEG_LISTING_STYLE = SEG_POST2005_SPOOL__FMT 
                            ELSE 
                              CALL ERR_LOG ( 6426, -2, 'GOUTPT', 'Wrong '// &
     &                            'value of SRC_STAT qualifier of the '// &
     &                            'LISTING_OPTIONS keyword '//TOKEN(1:12)// &
     &                            ' -- only values PRE2005, POST2005 '// &
     &                            'are acceptable' )
                              STOP 'BATCH(goutpt)  abnormal termination'
                         END IF
                      ELSE
                         CALL ERR_LOG ( 6427, -2, 'GOUTPT', 'Wrong '// &
     &                       'LISTING_OPTIONS value: '//TOKEN(1:12)// &
     &                       ' -- only values SRC_STAT is acceptable' )
                        STOP 'BATCH(goutpt)  abnormal termination'
                    END IF
                    STR = STRING
                    CALL CHASHL ( STR ) 
                    IF ( STR(1:1) .EQ. '\' ) THEN
                         CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                       ELSE
                         GOTO 810
                    END IF
 410             CONTINUE 
 810             CONTINUE 
                 KLOPT = .TRUE.
              ELSE IF ( TOKEN .EQ. 'NRD_TABLE' ) THEN
                 IF ( KNRD ) CALL FERR ( INT2(6180), &
     &               'BATCH(goutpt) NRD_TABLE '//'used twice', INT2(0), INT2(0) )
                 CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'YES' ) THEN
                      FL_NRD_TABLE = .TRUE.
                    ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                      FL_NRD_TABLE = .FALSE.
                    ELSE
                      CALL ERR_LOG ( 6428, -2, 'GOUTPT', 'Wrong '// &
     &                    'NRD_TABLE value: '//TOKEN(1:12)// &
     &                    ' -- only values YES or NO are acceptable' )
                      STOP 'BATCH(goutpt)  abnormal termination'
                 END IF
                 KNRD = .TRUE.
              ELSE IF ( TOKEN .EQ. 'CHI_SQUARE_TABLE' ) THEN
                 IF ( KCHI ) CALL FERR ( INT2(6190), 'BATCH(goutpt) '// &
     &               'CHI_SQUARE_TABLE used twice', INT2(0), INT2(0) )
                 CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'YES' ) THEN
                      FL_CHI_TABLE = .TRUE.
                    ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                      FL_CHI_TABLE = .FALSE.
                    ELSE
                      CALL ERR_LOG ( 6429, -2, 'GOUTPT', 'Wrong '// &
     &                    'CHI_SQUARE_TABLE value: '//TOKEN(1:12)// &
     &                    ' -- only values YES or NO are acceptable' )
                      STOP 'BATCH(goutpt)  abnormal termination'
                 END IF
                 KCHI = .TRUE.
              ELSE IF ( TOKEN .EQ. 'IONOSPHERIC_MODEL' ) THEN
!
! -------------- Parse the inospheric model parameters
!
                 IF ( KION ) CALL FERR ( INT2(6190), 'BATCH(goutpt) '// &
     &               'IONOSPHERIC_MODEL used twice', INT2(0), INT2(0) )
!
! -------------- Parse COLLECT clause
!
                 CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                 IF ( TOKEN(1:2) == 'NO' ) THEN
                      KION = .TRUE.
                   ELSE
                      IF ( TOKEN .NE. 'COLLECT' ) THEN
                           CALL ERR_LOG ( 6430, -2, 'GOUTPT', 'Wrong '// &
     &                         'qualifier  in IONOSPHERIC_MODEL: '//TRIM(TOKEN)// &
     &                         ' while COLLECT was expected' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .EQ. 'YES' ) THEN
                           GIM_COLLECT_INFO = .TRUE.
                         ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                           GIM_COLLECT_INFO = .FALSE.
                         ELSE
                           CALL ERR_LOG ( 6431, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualirer COLLECT in IONOSPHERIC_MODEL '// &
     &                         TRIM(TOKEN)//' -- only values YES or NO are acceptable' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
!
! ------------------- Parse BIAS_COMPUTE clause
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .NE. 'BIAS_COMPUTE' ) THEN
                           CALL ERR_LOG ( 6432, -2, 'GOUTPT', 'Wrong '// &
     &                         'qualifier  in IONOSPHERIC_MODEL: '//TRIM(TOKEN)// &
     &                         ' while BIAS_COMPUTE was expected' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .EQ. 'YES' ) THEN
                           GIM_EST = .TRUE.
                         ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                           GIM_EST = .FALSE.
                         ELSE
                           CALL ERR_LOG ( 6433, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualirer BIAS_COMPUTE in IONOSPHERIC_MODEL '// &
     &                         TRIM(TOKEN)//' -- only values YES or NO are acceptable' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
!
! ------------------- Parse BIAS_COMPUTE clause
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .NE. 'REGR_COMPUTE' ) THEN
                           CALL ERR_LOG ( 6434, -2, 'GOUTPT', 'Wrong '// &
     &                         'qualifier  in IONOSPHERIC_MODEL: '//TRIM(TOKEN)// &
     &                         ' while BIAS_COMPUTE was expected' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .EQ. 'YES' ) THEN
                           GIM_RGR = .TRUE.
                         ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                           GIM_RGR = .FALSE.
                         ELSE
                           CALL ERR_LOG ( 6435, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualirer REGR_COMPUTE in IONOSPHERIC_MODEL '// &
     &                         TRIM(TOKEN)//' -- only values YES or NO are acceptable' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
!
! ------------------- Parse DB_UPDATE clause
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .NE. 'DB_UPDATE' ) THEN
                           CALL ERR_LOG ( 6436, -2, 'GOUTPT', 'Wrong '// &
     &                         'qualifier  in IONOSPHERIC_MODEL: '//TRIM(TOKEN)// &
     &                         ' while DB_UPDATE was expected' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .EQ. 'YES' ) THEN
                           GIM_WRI = .TRUE.
                         ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                           GIM_WRI = .FALSE.
                         ELSE
                           CALL ERR_LOG ( 6437, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualirer WRITE in IONOSPHERIC_MODEL '// &
     &                         TRIM(TOKEN)//' -- only values YES or NO are acceptable' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      IF ( GIM_WRI .AND. .NOT. GIM_EST ) THEN
                           CALL ERR_LOG ( 6438, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualirer WRITE in IONOSPHERIC_MODEL '// &
     &                         TRIM(TOKEN)//' -- YES, but BIAS_COMPUTE = NO. DB_UPDATE = YES '// &
     &                         'requires BIAS_COMPUTE = YES' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
!
! ------------------- Parse IONO_INFO_DIR clause
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .NE. 'IONO_INFO_DIR' ) THEN
                           CALL ERR_LOG ( 6439, -2, 'GOUTPT', 'Wrong '// &
     &                         'qualifier  in IONOSPHERIC_MODEL: '//TRIM(TOKEN)// &
     &                         ' while IONO_INFO_DIR was expected' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
                      DIR_DESC = FUNC_OPENDIR ( TRIM(TOKEN)//CHAR(0) )
                      IF ( DIR_DESC .LE. 0 ) THEN
                           CALL ERR_LOG ( 6440, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualifier IONO_INFO_DIR in IONOSPHERIC_MODEL: '// &
     &                          TRIM(TOKEN)//' -- directory does not exist' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      IS = CLOSEDIR ( %VAL(DIR_DESC) )
                      GIM_INFO_DIR = TOKEN
!
! ------------------- Parse IONO_DTEC_DIR clause
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .NE. 'IONO_DTEC_DIR' ) THEN
                           CALL ERR_LOG ( 6441, -2, 'GOUTPT', 'Wrong '// &
     &                         'qualifier  in IONOSPHERIC_MODEL: '//TRIM(TOKEN)// &
     &                         ' while IONO_DTEC_DIR was expected' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
                      DIR_DESC = FUNC_OPENDIR ( TRIM(TOKEN)//CHAR(0) )
                      IF ( DIR_DESC .LE. 0 ) THEN
                           CALL ERR_LOG ( 6442, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualifier IONO_DTEC_DIR in IONOSPHERIC_MODEL: '// &
     &                          TRIM(TOKEN)//' -- directory does not exist' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      IS = CLOSEDIR ( %VAL(DIR_DESC) )
                      GIM_DTEC_DIR = TOKEN
!
! ------------------- Parse IONO_ADDW_DIR clause
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .NE. 'IONO_ADDW_DIR' ) THEN
                           CALL ERR_LOG ( 6443, -2, 'GOUTPT', 'Wrong '// &
     &                         'qualifier  in IONOSPHERIC_MODEL: '//TRIM(TOKEN)// &
     &                         ' while IONO_ADDW_DIR was expected' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
                      DIR_DESC = FUNC_OPENDIR ( TRIM(TOKEN)//CHAR(0) )
                      IF ( DIR_DESC .LE. 0 ) THEN
                           CALL ERR_LOG ( 6444, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualifier IONO_ADDW_DIR in IONOSPHERIC_MODEL: '// &
     &                          TRIM(TOKEN)//' -- directory does not exist' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      IS = CLOSEDIR ( %VAL(DIR_DESC) )
                      GIM_ADDW_DIR = TOKEN
!
! ------------------- Parse IONO_DEL_DIR clause
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .NE. 'IONO_DEL_DIR' ) THEN
                           CALL ERR_LOG ( 6445, -2, 'GOUTPT', 'Wrong '// &
     &                         'qualifier  in IONOSPHERIC_MODEL: '//TRIM(TOKEN)// &
     &                         ' while IONO_DEL_DIR was expected' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
                      DIR_DESC = FUNC_OPENDIR ( TRIM(TOKEN)//CHAR(0) )
                      IF ( DIR_DESC .LE. 0 ) THEN
                           CALL ERR_LOG ( 6446, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualifier IONO_DEL_DIR in IONOSPHERIC_MODEL: '// &
     &                          TRIM(TOKEN)//' -- directory does not exist' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      IS = CLOSEDIR ( %VAL(DIR_DESC) )
                      GIM_DEL_DIR = TOKEN
!
! ------------------- Parse IONO_NOI_DIR clause
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .NE. 'IONO_NOI_DIR' ) THEN
                           CALL ERR_LOG ( 6447, -2, 'GOUTPT', 'Wrong '// &
     &                         'qualifier  in IONOSPHERIC_MODEL: '//TRIM(TOKEN)// &
     &                         ' while IONO_NOI_DIR was expected' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
                      DIR_DESC = FUNC_OPENDIR ( TRIM(TOKEN)//CHAR(0) )
                      IF ( DIR_DESC .LE. 0 ) THEN
                           CALL ERR_LOG ( 6448, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualifier IONO_NOI_DIR in IONOSPHERIC_MODEL: '// &
     &                          TRIM(TOKEN)//' -- directory does not exist' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      IS = CLOSEDIR ( %VAL(DIR_DESC) )
                      GIM_NOI_DIR = TOKEN
!
! ------------------- Parse BCL_FIL clause
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .NE. 'BCL_FIL' ) THEN
                           CALL ERR_LOG ( 6449, -2, 'GOUTPT', 'Wrong '// &
     &                         'qualifier  in IONOSPHERIC_MODEL: '//TRIM(TOKEN)// &
     &                         ' while BCL_FIL was expected' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
                      INQUIRE ( FILE=TOKEN, EXIST=LEX )
                      IF ( .NOT. LEX ) THEN
                           CALL ERR_LOG ( 6450, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualifier BCL_FIL in IONOSPHERIC_MODEL: '// &
     &                          TRIM(TOKEN)//' -- file does not exist' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      BCL_FIL = TOKEN
!
! ------------------- Parse BRC_FIL clause
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .NE. 'BRK_FIL' ) THEN
                           CALL ERR_LOG ( 6451, -2, 'GOUTPT', 'Wrong '// &
     &                         'qualifier  in IONOSPHERIC_MODEL: '//TRIM(TOKEN)// &
     &                         ' while BRK_FIL was expected' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
                      INQUIRE ( FILE=TOKEN, EXIST=LEX )
                      IF ( .NOT. LEX ) THEN
                           CALL ERR_LOG ( 6452, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualifier BRK_FIL in IONOSPHERIC_MODEL: '// &
     &                          TRIM(TOKEN)//' -- file does not exist' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      BRK_FIL = TOKEN
!
! ------------------- Parse GIM_MODE clause
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .NE. 'GIM_MODE' ) THEN
                           CALL ERR_LOG ( 6453, -2, 'GOUTPT', 'Wrong '// &
     &                         'qualifier  in IONOSPHERIC_MODEL: '//TRIM(TOKEN)// &
     &                         ' while GIM_MODE was expected' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      READ ( UNIT=TOKEN, FMT='(I1)', IOSTAT=IOS ) GIM_MODE
                      IF ( IOS .NE. 0 ) THEN
                           CALL ERR_LOG ( 6454, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualifier GIM_MODE in IONOSPHERIC_MODEL: '// &
     &                         'an integer was expected, but got '//TOKEN )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      IF ( GIM_MODE < 0 .OR. GIM_MODE > 3 ) THEN
                           CALL ERR_LOG ( 6455, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualifier GIM_MODE in IONOSPHERIC_MODEL: '// &
     &                         'an integer in a range of [0, 3] was expected, but got '//TOKEN )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
!
! ------------------- Parse GIM_DEG clause
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .NE. 'GIM_DEG' ) THEN
                           CALL ERR_LOG ( 6456, -2, 'GOUTPT', 'Wrong '// &
     &                         'qualifier  in IONOSPHERIC_MODEL: '//TRIM(TOKEN)// &
     &                         ' while GIM_DEG  was expected' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      READ ( UNIT=TOKEN, FMT='(I1)', IOSTAT=IOS ) GIM_DEG
                      IF ( IOS .NE. 0 ) THEN
                           CALL ERR_LOG ( 6457, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualifier GIM_DEG in IONOSPHERIC_MODEL: '// &
     &                         'an integer was expected, but got '//TOKEN )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      IF ( GIM_DEG < 0 .OR. GIM_DEG > 3 ) THEN
                           CALL ERR_LOG ( 6458, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualifier GIM_DEG in IONOSPHERIC_MODEL: '// &
     &                         'an integer in a range of [0, 3] was expected, but got '//TOKEN )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
!
! ------------------- Parse GIM_TIM_STEP clause
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .NE. 'GIM_TIM_STEP' ) THEN
                           CALL ERR_LOG ( 6459, -2, 'GOUTPT', 'Wrong '// &
     &                         'qualifier  in IONOSPHERIC_MODEL: '//TRIM(TOKEN)// &
     &                         ' while GIM_TIM_STEP was expected' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      READ ( UNIT=TOKEN, FMT='(F10.5)', IOSTAT=IOS ) GIM_TIM_STEP
                      IF ( IOS .NE. 0 ) THEN
                           CALL ERR_LOG ( 6460, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualifier GIM_TIM_STEP in IONOSPHERIC_MODEL: '// &
     &                         'an integer was expected, but got '//TOKEN )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      IF ( GIM_TIM_STEP < 60.0 .OR. GIM_TIM_STEP > 90000.0 ) THEN
                           CALL ERR_LOG ( 6461, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualifier GIM_TIM_STEP in IONOSPHERIC_MODEL: '// &
     &                         'an integer in a range of [60, 90000] was expected, but got '//TOKEN )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
!
! ------------------- Parse GIM_SCALE clause
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .NE. 'GIM_SCALE' ) THEN
                           CALL ERR_LOG ( 6462, -2, 'GOUTPT', 'Wrong '// &
     &                         'qualifier  in IONOSPHERIC_MODEL: '//TRIM(TOKEN)// &
     &                         ' while GIM_SCALE was expected' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      READ ( UNIT=TOKEN, FMT='(F10.5)', IOSTAT=IOS ) GIM_SCALE
                      IF ( IOS .NE. 0 ) THEN
                           CALL ERR_LOG ( 6463, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualifier GIM_SCALE in IONOSPHERIC_MODEL: '// &
     &                         'an integer was expected, but got '//TOKEN )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      IF ( GIM_SCALE < 0.001 .OR. GIM_SCALE > 5.0 ) THEN
                           CALL ERR_LOG ( 6464, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualifier GIM_SCALE in IONOSPHERIC_MODEL: '// &
     &                         'an integer in a range of [0.001, 5.0] was expected, but got '//TOKEN )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
!
! ------------------- Parse GIM_SEED clause
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .NE. 'GIM_SEED' ) THEN
                           CALL ERR_LOG ( 6465, -2, 'GOUTPT', 'Wrong '// &
     &                         'qualifier  in IONOSPHERIC_MODEL: '//TRIM(TOKEN)// &
     &                         ' while GIM_SEED was expected' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      READ ( UNIT=TOKEN, FMT='(I12)', IOSTAT=IOS ) GIM_SEED
                      IF ( IOS .NE. 0 ) THEN
                           CALL ERR_LOG ( 6466, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualifier GIM_SEED in IONOSPHERIC_MODEL: '// &
     &                         'an integer was expected, but got '//TOKEN )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      IF ( GIM_SEED < 1 ) THEN
                           GIM_SEED = TIME( %VAL(0) )
                      END IF
!
! ------------------- Parse GIM_VERB clause
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .NE. 'GIM_VERB' ) THEN
                           CALL ERR_LOG ( 6468, -2, 'GOUTPT', 'Wrong '// &
     &                         'qualifier  in IONOSPHERIC_MODEL: '//TRIM(TOKEN)// &
     &                         ' while GIM_VERB  was expected' )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      READ ( UNIT=TOKEN, FMT='(I1)', IOSTAT=IOS ) GIM_VERB
                      IF ( IOS .NE. 0 ) THEN
                           CALL ERR_LOG ( 6469, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualifier GIM_VERB in IONOSPHERIC_MODEL: '// &
     &                         'an integer was expected, but got '//TOKEN )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
                      IF ( GIM_VERB < 0 .OR. GIM_VERB > 99 ) THEN
                           CALL ERR_LOG ( 6470, -2, 'GOUTPT', 'Wrong '// &
     &                         'value of the qualifier GIM_VERB in IONOSPHERIC_MODEL: '// &
     &                         'an integer in a range of [0, 99] was expected, but got '//TOKEN )
                           STOP 'BATCH(goutpt)  abnormal termination'
                      END IF
!
                      KION = .TRUE.
                 END IF
              ELSE
!
! -------------- Something that isn't suppose to be there
!
                 CALL FERR ( INT2(6200), 'BATCH(goutpt) unknown keyword '// &
     &                TOKEN(1:16), INT2(0), INT2(0) )
            ENDIF ! token
         ENDDO
         LENGTH=CFREAD(STRING)
      ENDDO
!
! --- Now that this section is finished, what now?
!
      IF ( .NOT. (KRST .AND. KMIN .AND. KBAS .AND. KFWD .AND. &
     &            KSCN .AND. KCVR)                            ) THEN
           CALL FERR ( INT2(6095), 'BATCH(goupt) missing some mandatory '// &
     &         'keywords from $OUTPUT', INT2(0), INT2(0) )
        ELSE
           CALL CFUNRD ( LENGTH, STRING )
      ENDIF
!
      IF ( ( COR_GL_FLAG .OR. COR_LL_FLAG .OR. COR_CL_FLAG ) .AND. &
     &     FAST_COV .EQ. F__GLO ) THEN
!
           CALL ERR_LOG ( 6471, -2, 'GOUTPT', 'Conflicts values '// &
     &         'FAST_COV in CORRELATIONS: set FAST_COV LOC or SEG if '// &
     &         'you really want to compute correlations' )
           STOP 'BATCH(goutpt) Abnormal termination'
      END IF
!
      IF ( FL_SINEX_MAKE .AND. TRAIN ) THEN
           CALL ERR_LOG ( 6472, -2, 'GOUTPT', 'Solve cannot '// &
     &         'write solution listing in sinex format in train mode. '// &
     &         'Please specify "TRAIN NO" in the control file' )
           STOP 'BATCH(goutpt) Abnormal termination'
      END IF
!    
      IF ( FL_SINEX_MAKE .AND. .NOT. FL_OVERWRITE ) THEN
           INQUIRE ( FILE=SINEX_OUTFILE, EXIST=LEX ) 
           IF ( LEX ) THEN
                CALL ERR_LOG ( 6473, -2, 'GOUTPT', 'Output listing in '// &
     &         'SINEX format '//SINEX_OUTFILE(1:I_LEN(SINEX_OUTFILE))// &
     &         ' already exists' )
           STOP 'BATCH(goutpt) Abnormal termination'
           END IF
      END IF
!
      CALL USE_GLBFIL_4 ( 'OWC' )
      RETURN
      END   !#!  GOUTPT  #!#
