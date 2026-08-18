      SUBROUTINE GSETUP ( RUN_INITS, SOLTYP, CGMNMR, INCGM_TYPE, &
     &                    INCGM_USER, INCGM_SOL, &
     &                    B_ARCDIR, ID, USER_PROG, USER_BUFF, &
     &                    KPERMARC, WEIGHTS, LF_WEI, WEIGHT_FILE, OUTCGM, &
     &                    USER_TAG, SOL_TAG, SOLARCH_SOL, KUSER_PART, &
     &                    USER_PART_PROG, KUSER_CONST, USER_CONST_PROG, CMERG, &
     &                    WEIGHT_TYPE_MA, WEIGHT_ALGORITHM, PARU_FILE, &
     &                    MF_WEI, IUER )
      IMPLICIT   NONE 
!
! 1.  GSETUP PROGRAM SPECIFICATION
!
! 1.1 Parse setup section.
!
! 1.2 REFERENCES:
!
! 2.  GSETUP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'precm.i'
      INCLUDE 'fast.i'
      INCLUDE 'baccm.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*2 RUN_INITS
!
!       run_inits - initials of globl run
!
! 2.3 OUTPUT Variables:
!
      CHARACTER SOLTYP*1
      CHARACTER CGMNMR*(*), ID(10)*(*), USER_PROG*(*), USER_BUFF*(*), &
     &          WEIGHTS*(*), B_ARCDIR(3)*(*), OUTCGM*(*), WEIGHT_FILE(*)*(*), &
     &          USER_PART_PROG*(*), USER_CONST_PROG*(*), CMERG(10)*(*), &
     &          WEIGHT_TYPE_MA*(*)
      CHARACTER INCGM_TYPE*4, USER_TAG*2, INCGM_USER*2, SOL_TAG*8, &
     &          INCGM_SOL*8, WEIGHT_PARSE*11
      LOGICAL*2 KPERMARC, SOLARCH_SOL, KUSER_PART, KUSER_CONST, &
     &          KFAST_MODE, KFAST_DBG, KFAST_COV, KSOLVE_EMULATION, &
     &          KSAVING_RATE, KQUALCODE_LIMIT, KSUPMET
      CHARACTER FILNAM*63, FAST_MODE_TAG*80, FAST_DBG_TAG*80, &
     &          FAST_COV_TAG*80, SOLVE_EMULATION_TAG*80, &
     &          SAVING_RATE_TAG*80, SUPMET_TAG*12, QUALCODE_LIMIT_TAG*12, &
     &          WARNING_TAG*16, STRING_SAVE*16384, SOLVE_VAR*128
      CHARACTER PARU_FILE*(*)
      INTEGER*4 WEIGHT_ALGORITHM, LF_WEI, MF_WEI
!
! B_ARCDIR       - Directories in which to write arc files
! ID             - String identifying the run
! KPERMARC       - True if arc files are to be permanently saved
! SOLTYP         - Solution type (complete,forward,back,independent,suppression)
! USER_BUFF      - User buffer
! USER_PROG      - User-specified program name
! WEIGHTS        - Usage option for weights (use,require,make,append, in or no_)
! LF_WEI         - the number of weight files
! WEIGHT_FILE    - Name of file containing weights
! WEIGHT_TYPE_MA - Type of weights:
!                  "B" -- baseline (default),
!                  "A" -- by_Arc (also called global),
!                  "S" -- by_site
! CGMNMR     - full path to input cgm (or NONE, if no input cgm)
! incgm_type - code for type of input cgm (NONE, specified as full PATH,
!              or specified by user and solution TAGS)
! incgm_user, incgm_sol - if input cgm type is TAGS, these variables give
!                         the specific tags
! OUTCGM      - file name of output cgm
! user_tag    - owner of this solution (if solution is to be cataloged)
! sol_tag     - solution tag of this solution (if solution is to be cataloged)
!               (515a etc.)
! solarch_sol - indicates whether or not this solution will be cataloged
!               (true for will be cataloged)
! WEIGHT_ALGORITHM -- identifier of the algorithm to be used in making weights
! ASM_NAM           -- Atmosphere stochastic model name
! ASM_STA_FIL       -- Atmosphere stochastic model station parameter file
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ctrfl
!       CALLED SUBROUTINES: cfread,cfunrd
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4  FC_GETENV
      ADDRESS__TYPE :: PTR_NC, PTR_CH
      CHARACTER  DELIMIT*1, STRING*16384, TOKEN*256, STR*32
      INTEGER*2  LENGTH, IDUM, I, J, LIM, IDCNT, &
     &           CGML_1, CGML_2, CGML_3, JERR, MERGCNT
      LOGICAL*2  KSOLUT, KCGM, KARC, KID, KUSPR, KWAIT, KSLTAG, KUSTAG
      LOGICAL*2  KUSPP, KUSCN, KSWAIT, KELDEP, KMERG, KSNGCHK, &
     &           KTRAIN, KSORT, KWARNING, KDEF, KEDC, KTPD, KAOC, &
     &           KTEC, KSNR, KASM, KION, KDTEC
      CHARACTER  CGMNMR1*64, CGMNMR2*64, CGMNMR3*64, EMESSAGE*255, &
     &           ARCFL_STRING*128, BUF_STR*1024
      INTEGER*2  ARCSTR_LEN, IARCFL_STRING(64), ICT, IPATH
      EQUIVALENCE ( ARCFL_STRING, IARCFL_STRING )
      LOGICAL*4  LEX
      LOGICAL*2  ARCSTR_ENV, KPER_TEMP
      CHARACTER  BARC_TEMP(3)*(NAME_SIZE)
      TYPE       ( TCN__TYPE ) :: TCN(M__TCN)
      INTEGER*2  J4, IB, IE
      INTEGER*4  J1, J2, J3, J5, J6, IP, IER, L_TCN
      ADDRESS__TYPE :: DIR_DESC
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IFIND_PL
      ADDRESS__TYPE, EXTERNAL :: OPENDIR
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   kdb  951215  New feature: arcfile directory environment variable
!                             containing a string analogous to the
!                             arc_files keyword.
!   kdb  951219  Add / to end of arc directory names if forgotten by the user.
!   jmg  960610  Access new user-partial chain utility.
!   kdb  970115  If the program can't find the input cgm, abort even in
!                foreground mode.  This addresses a problem in which a user
!                forged on, then realized the problem, panicked and killed the
!                program when he wasn't supposed to, leaving a partial pending
!                entry.  The goal is to minimize obscure catalog errors that
!                can strand the user, even if they could be manually avoided
!                by the users.
!   pet  970128  Two new keywords were added: FAST_MODE, FAST_DBG
!   kdb  970204  New site weighting feature. (Requires new WEIGHTS keyword
!                option to specify weight type (by_arc or by_site)).
!   pet  970304  New keyword was added: FAST_COV
!   pet  970513  Initialization of FAST_xxx_GLO variuables added
!   pet  970711  Initialization of CHI_TOL and TIGHTNESS added
!   pet  970717  Blocked work when attempt to make weight file in fast mode and
!                FAST_COV is not F__SEG
!   pet  970823  Added defining GAMB global variables
!   pet  970917  Added test of conflicting options (fast_mode and solution)
!   pet  971003  SOLVE_EMULATION keyword added.
!   pet  971006  Disabled FAST_COV default and made keyword FAST_COV mandatory,
!                but small kludge left for the compatibility with the previous
!                versions: if FAST_MODE = F__NONE ( explicitly or as default
!                value), then FAST_COV is set up F__FUL and test for the
!                presence of the FAST_COV keyword is not applied.
!   pet  971010  Enhanced test of conflicting options (fast_mode and solution)
!   pet  980204  Added support of the keyword   SAVING_RATE.
!   pet  980508  Added support of the keywords  CRES_COMPAT, QUALCODE_LIMIT,
!                SUPMET
!   pet  980708  Added support of new mandatory keywords SINGULARITY_CHECK
!   pet  980722  Added setting default values of sigmas of constraints
!   pet  980722  Added setting default values of sigmas of constraints
!   pet  990406  Added support of WARNING keyword
!   pet  990409  Added check of conflicting options TRAIN and SOLUTION
!   pet  1999.10.07 Added adjusting ID string to the left edge.
!   pet  2000.03.29 Changed syntax of keyword WEIGHTS: added support qualifier
!                   ALGOTITHM
!   pet  2000.05.01 Added support of a vlaue NONE for keywords USER_PROGRAM,
!                   USER_PARTILS, USER_CONSTRAINTS
!   pet  2000.05.10 Added limited support of DEFAULTS keyword
!   pet  2000.05.11 Added support qualifies WEIGHTS NO and WEIGHTS IN
!   pet  2000.07.27 Added trap of internal control: NO TRAIN mode and
!                   WEIGHTS (MAKE or APPEND) ALGORIGTHM MYWAY are incompatible.
!   pet  2000.09.20 Corrected a bug: if wieght_file was specified without
!                   absolute path then $SAVE_DIR is prepended before the actual
!                   name
!   pet  2000.10.23 Added a call of set_merg in order to updated field mergcgm
!                   in glbcm.i which is different from the argument mergcgm!!
!                   renamed mergcgm to cmerg
!   pet  2001.05.10 Change the logic of the keyword CGM:
!                   if the output CGM was specified as NONE then
!                   a) the old logic: temporarily CGM was written;
!                   b) The new logic: no CGM is written.
!                   if the output CGM was specified as SAVE then
!                   a) the old logic: CGM with name SAVE was written
!                   b) The new logic: temporarily CGM is written.
!   pet 2001.12.13  Added support of a new type of solutions: GLOBAL_ONLY
!                   ( SOLTYP = 'G' )
!   pet 2002.03.07  Resolved inconsistency with user_buffer option:
!                   the previous version considred USER_BUFFER as a keyword.
!                   The new version considers USER_BUFFER as a qualifier of
!                   USER_PROGRAM keyword. "\" separator is allowed.
!   pet 2002.03.28  Disabled support of keywrods SOLVE_COMPAT, CRES_COMPAT
!   pet 2006.02.09  Added support of WEIGHTS INSERT option
!   pet 2007.06.01  Added support of ELIM algorithm for WEIGHTS keyword
!   pet 2007.10.25  Added support of DECIMATION keyword
!   pet 2007.11.06  Added support of ... something good
!   pet 2008.04.23  Added support of APRIORI_OBS_CORR keyword
!   pet 2009.02.21  Increased MF_WEI
!   pet 2009.06.01  Added support of qualitfies MULT_GLOBAL for the
!                   keyword ELEVATION_DEPENDENT_NOISE
!   pet 2010.10.21  Added support of the new keyword GPS_TEC_NOISE
!   pet 2013.06.11  Added support of the new keyword SNR_MIN
!   pet 2016.12.25  Added support of EXTERNAL_IONO_MODEL. It was later renamed to DTEC_USE
!   pet 2019.09.03  Changed type of  DIR_DISC and OPENDIR to INTEGER*8
!   pet 2021.03.16  Changed processing APRIORI_OBS_CORR keyword
!   pet 2021.05.17  Added support of   ATMOSPHERE_STOCHASTIC_MODEL
!   pet 2021.12.30  Remobed support of APRIORI_OBS_CORR keyword
!   pet 2023.09.22  Added support of commands DEFINE and UNDEFINE 
!
! 5.  GSETUP PROGRAM STRUCTURE
!
!  EXTERNAL FUNCTIONS
!
!CCCCC
      INTEGER*2  CFREAD, TRIMLEN
      LOGICAL*2  CFEOF
      INTEGER*4  TIM_ARR_I4(16), IUER
!
! --- Initialization
!
      DATA KSOLUT/.FALSE./, KCGM/.FALSE./,   KARC/.FALSE./,  KID/.FALSE./
      DATA KUSPR /.FALSE./, KWAIT/.FALSE./, KUSPP/.FALSE./
      DATA KSLTAG/.FALSE./, KUSTAG/.FALSE./, KUSCN/.FALSE./, KMERG /.FALSE./
      DATA KSWAIT/.FALSE./, KELDEP/.FALSE./, KDEF /.FALSE./, KAOC  /.FALSE./ 
!
      KFAST_MODE       = .FALSE.
      KFAST_DBG        = .FALSE.
      KFAST_COV        = .FALSE.
      KSOLVE_EMULATION = .FALSE.
      KSAVING_RATE     = .FALSE.
      KSNGCHK          = .FALSE.
      KSUPMET          = .FALSE.
      KQUALCODE_LIMIT  = .FALSE.
      KTRAIN           = .FALSE.
      KSORT            = .FALSE.
      KWARNING         = .FALSE.
      KEDC             = .FALSE.
      KASM             = .FALSE.
      KTPD             = .FALSE.
      KDEF             = .FALSE.
      KTEC             = .FALSE.
      KSNR             = .FALSE.
      KION             = .FALSE.
      KDTEC            = .FALSE.
      DEFAULTS_USE     = .TRUE.
      CALL CLRCH ( CGMNMR  )
      CALL CLRCH ( CGMNMR1 )
      CALL CLRCH ( CGMNMR2 )
      CALL CLRCH ( CGMNMR3 )
      CALL CLRCH ( USER_BUFF )
      CALL CLRCH ( PARU_FILE )
!
! --- Main loop
!
      CALL USE_GLBFIL   ( 'OR' )
      CALL USE_GLBFIL_4 (  'R' )
!
      KBEEP = .FALSE. ! Else BATCH call curses routine what lead to bus-error
      KUSER_PART  = .FALSE.
      KUSER_CONST = .FALSE.
      WEIGHTS     = 'D'  ! skips weighting feature
      DO I = 1,8
         WEIGHT_FILE(I) = ' '
      END DO
      WEIGHT_ALGORITHM = WEIGHT__NONE
      SNGCHK_ACTION    = SNGCHK_ACTION__DEF
      RWT_EL_USE       = SOLVE__NO
      RWT_SRC_USE      = SOLVE__NO
      RWT_EL_GLB       = 0.0D0
      CALL CLRCH ( RWT_SRC_FIL    )
      CALL CLRCH ( RWT_STA_EL_FIL )
      CALL CLRCH ( AOC_FIL )
      CALL CLRCH ( ADDW_FIL )
      CALL CLRCH ( EDIT_FIL )
      CALL CLRCH ( TEC_NOISE_FILE )
      FL_NO_IONO_DEL   = .FALSE.
      L_TCN = 0
      CALL CLRCH ( IONOV_DIR )
      IONOV_USE = IONOV__NONE
      DTEC_USE  = DTEC__NONE
!
! --- Using default (baseline) weighting.
!
      WEIGHT_TYPE_MA     = 'B'
      CALL CLRCH ( SOURCE_WEIGHT_FILE )
      CALL CLRCH ( OUTCGM             )
      IDCNT = 0
      DO I=1,10
         CALL CLRCH ( ID(I)      )
         CALL CLRCH ( CMERG(I) )
      ENDDO
      MERGCNT = 0
      KELDEP_NOISE  = .FALSE.
      ELDEP_FILE    = ' '
      SOLARCH_SOL   = .FALSE.
!
      FAST_MODE_GLO = FAST_MODE__DEF  !  Default
      FAST_DBG_GLO  = FAST_DBG__DEF   !  Default
      FAST_COV_GLO  = F__FUL          !  Default
!
      FAST_MODE     = FAST_MODE__DEF  !  Default
      FAST_DBG      = FAST_DBG__DEF   !  Default
      FAST_COV      = F__FUL          !  Default
!
      SOLVE_EMULATION   = SOLVE_EMULATION__DEF
      SAVING_RATE       = SAVING_RATE__DEF
      SUPMET_BAT        = SUPMET__UND
      QUALCODE_GOOD_LIM = QUALCODE_GOOD_LIM__DEF
      G_WARNING         = G_WARNING__DEF
      GLO_SNR_MIN_X     = 0.0D0
      GLO_SNR_MIN_S     = 0.0D0
      SESS_REWEI_SCALE  = 1.0D0
      SESS_REWEI_QUADR  = 0.0D0
      CALL DATE_AND_TIME( STR, STR, STR, TIM_ARR_I4 )
!
! --- Initialize the random number seed: 
! --- thge summ of the number of days since Year 0 and the 
! --- number of milliseconds since the midnight
!
      SEED_INIT_I4 = (TIM_ARR_I4(1)*365.25  + TIM_ARR_I4(2)*30.0 + &
     &                TIM_ARR_I4(3)) + &
     &               (TIM_ARR_I4(5)*3600.0  + TIM_ARR_I4(6)*60.0 + &
     &                TIM_ARR_I4(7))*1000.0 + TIM_ARR_I4(8)
!
! --- Set default sigmas of constraints
!
      CALL CNS_DEFAULT()
!
! --- Defining GAMB global variables
!
      CALL ERR_PASS( IUER, IER )
      CALL GAMB_DEFAULT ( IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8661, IUER, 'GSETUP', 'Error in defining GAMB '// &
     &         'environment variables' )
           RETURN 
      END IF
!
! --- Defining REWAY global variables
!
      CALL ERR_PASS( IUER, IER )
      CALL REWAY_DEFAULT ( IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8662, IUER, 'GSETUP', 'Error in defining REWAY '// &
     &         'environment variables' )
           RETURN 
      END IF
!
      TRAIN    = TRAIN__DEF
      SORT_SOU = SORT_SOU__DEF
      SORT_STA = SORT_STA__DEF
!
      CALL CLRCH ( EDC_DIR )
      EDC_USE    = EDC__UNDF
      EDC_PAR    = 0
      TPD_FLAG   = TPD__UNDF
      N_TPD_INIT = 0
      TPD_FLAG   = TPD__UNDF
      CALL CLRCH ( TPD_DIR )
      CALL CLRCH ( TPD_INIT_LIST )
      CALL CLRCH ( ASM_NAM )
      CALL CLRCH ( ASM_STA_FIL )
!
      CALL USE_GLBFIL   ( 'W'  )
      CALL USE_GLBFIL_4 ( 'WC' )
!
! --- Check for an optional arcfile directory environment variable.
! --- This must be a string analogous to and formatted like the
! --- arc_files keyword.
!
      ARCSTR_ENV = .FALSE.
      ARCSTR_LEN = FC_GETENV(PTR_CH('ARC_STR'//CHAR(0)), PTR_NC(IARCFL_STRING) )
      IF ( ARCSTR_LEN .GT. 0 ) THEN
           ARCSTR_ENV = .TRUE.
           DO ICT = 1,128
              IF ( ARCFL_STRING(ICT:ICT) .EQ. CHAR(0) ) THEN
                   ARCFL_STRING(ICT:ICT) = ' '
              END IF
           ENDDO
           CALL PARSE_ARCSTR ( ARCFL_STRING(1:ARCSTR_LEN), KPERMARC, B_ARCDIR )
      ENDIF
!
! --- Read a line of the control file
!
      LENGTH=CFREAD(STRING)
      DO WHILE ( STRING(1:1) .EQ. ' '   .AND. .NOT. CFEOF(IDUM) )
         DO WHILE(TRIMLEN(STRING).GT.0)
            CALL SPLITSTRING(STRING,TOKEN,STRING )
            IF ( TOKEN .EQ. 'DEFAULTS' ) THEN
                 IF ( KDEF ) CALL FERR ( INT2(3000), &
     &               'Gsetup(BATCH) Keyword '//'DEFAULTS used twice', INT2(0), &
     &                INT2(0) )
                 CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'YES' ) THEN
                      DEFAULTS_USE = .TRUE.
                    ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                      DEFAULTS_USE = .FALSE.
                    ELSE
                      CALL FERR ( INT2(3002), &
     &                    'GSETUP(BATCH) Unkown qualifier in '// &
     &                    ' DEFAULTS keyword: '//TOKEN(1:16)// &
     &                    'one of NO or YES was expected', INT2(0), INT2(0) )
                      STOP 'BATCH: abnormal termination'
                 END IF
                 KDEF = .TRUE.
             ELSE IF ( TOKEN .EQ. 'SOLUTION' ) THEN
!
! ------------- 'SOLUTION' keyword
!
                SUBTRACT_ARC = .FALSE.
                IF ( KSOLUT ) CALL FERR ( INT2(3010), &
     &              'Gsetup(BATCH) Keyword '//'SOLUTION used twice', INT2(0), &
     &               INT2(0) )
                CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                IF ( TOKEN .EQ. 'FORWARD' ) THEN
                     SOLTYP='F'
                  ELSE IF(TOKEN.EQ.'BACK') THEN
                    SOLTYP='B'
                  ELSE IF(TOKEN.EQ.'INDEPENDENT') THEN
                    SOLTYP='I'
                  ELSE IF ( TOKEN .EQ. 'SUPPRESSION' .OR. &
     &                      TOKEN .EQ. 'SUPPRESS'         ) THEN
                    SOLTYP='S'
                  ELSE IF ( TOKEN .EQ. 'COMPLETE' ) THEN
                    SOLTYP='C'
                  ELSE IF ( TOKEN .EQ. 'GLOBAL_ONLY' ) THEN
                    SOLTYP='G'
                  ELSE IF(TOKEN.EQ.'SUBTRACT') THEN
                    SOLTYP='F'
                    SUBTRACT_ARC = .TRUE.
                  ELSE
                    CALL FERR ( INT2(3012), &
     &                  'GSETUP(BATCH) Unkown solution type '//TOKEN(1:16), &
     &                   INT2(0), INT2(0) )
                ENDIF
                KSOLUT=.TRUE.
                CALL USE_GLBFIL_4 ( 'OWC' )
             ELSE IF(TOKEN.EQ.'CGM') THEN
!
! ------------ 'CGM' keyword
!
               IF ( KCGM ) CALL FERR ( INT2(3020), 'CGM USED TWICE', INT2(0), &
     &              INT2(0) )
               CALL SPLITSTRING ( STRING, CGMNMR1, STRING )
               IF ( TRIMLEN(CGMNMR1) .LE. 0 ) CALL FERR ( INT2(3022), &
     &             'BATCH(GSETUP) NO CGM SPECIFIED', INT2(0), INT2(0) )
               CALL SPLITSTRING ( STRING, CGMNMR2, STRING )
               CALL SPLITSTRING ( STRING, CGMNMR3, STRING )
               KCGM=.TRUE.
          ELSE IF ( TOKEN .EQ. 'MERGE_CGM' ) THEN
!
! --------- MERGE_CGM
!
            MERGCNT = MERGCNT+1
            CALL SPLITSTRING ( STRING, CMERG(MERGCNT), STRING )
            IF ( TRIMLEN(CMERG(MERGCNT)) .LE. 0 ) THEN
                 CALL FERR( INT2(3055), 'NO MERGE_CGM SPECIFIED', INT2(0), &
     &                INT2(0) )
            END IF
            IF ( CMERG(MERGCNT) .EQ. 'NONE' ) THEN
                 CALL CLRCH ( CMERG(MERGCNT) )
                 MERGCNT = MERGCNT-1
            ENDIF
            KMERG=.TRUE.
          ELSE IF ( TOKEN .EQ. 'ARC_FILES' ) THEN
!
! --------- 'ARC_FILES' keyword
!
            IF(KARC) CALL FERR( INT2(3030), 'ARC_FILES USED TWICE', INT2(0), &
     &         INT2(0) )
            CALL PARSE_ARCSTR(STRING,KPER_TEMP,BARC_TEMP )
            IF (.NOT.ARCSTR_ENV) THEN
              KPERMARC = KPER_TEMP
              DO ICT = 1,3
                B_ARCDIR(ICT) = BARC_TEMP(ICT)
              END DO
            ENDIF
            KARC=.TRUE.
          ELSE IF(TOKEN.EQ.'ID') THEN
!
! --------- 'ID' keyword
!
            idcnt = idcnt + 1
            IF(idcnt.gt.10) CALL FERR( INT2(3040), 'TOO MANY ID LINES', &
     &         INT2(0), INT2(0) )
            ID(idcnt)=STRING(1:len(id))
            CALL CHASHL ( ID(IDCNT) ) ! Adjust to the lsft edge
!            STRING(1:len(id))=' '
            STRING =' '
            IF(TRIMLEN(ID).LE.0) THEN
              CALL FERR( INT2(3042), 'NO ID SPECIFIED', INT2(0), INT2(0) )
            ENDIF
            KID=.TRUE.
          ELSE IF ( TOKEN .EQ. 'USER_PROGRAM' ) THEN
!
! --------- USER_PROGRAM
!
            IF ( KUSPR ) CALL FERR ( INT2(3050), 'GSETUP(BATCH) Keyword '// &
     &          'USER_PROGRAM use twice', INT2(0), INT2(0) )
            CALL SPLIT_STRING ( STRING, USER_PROG, STRING )
            IF ( TRIMLEN ( USER_PROG ) .LE. 0 ) THEN
                 CALL FERR ( INT2(3052), &
     &               'GSETUP(BATCH) no USER_PROG SPECIFIED', INT2(0), INT2(0) )
            ENDIF
!
            IF ( USER_PROG .EQ. 'NONE' .OR. USER_PROG .EQ. 'NO' .OR. &
     &           USER_PROG .EQ. 'none' .OR. USER_PROG .EQ. 'no' ) THEN
                 CALL CLRCH ( USER_PROG )
            END IF
            KUSPR=.TRUE.
!
! --------- Now try to extract user buffer
!
            CALL SPLIT_STRING ( STRING, BUF_STR, STRING )
            IF ( BUF_STR == '\' ) THEN
                 LENGTH = CFREAD ( STRING )
                 CALL SPLIT_STRING ( STRING, BUF_STR, STRING )
            END IF
!
            IF ( BUF_STR(1:1) .EQ. ' ' ) THEN
                 CONTINUE
               ELSE IF ( BUF_STR .EQ. 'USER_BUFFER' ) THEN
                 BUF_STR = STRING ! Copy the string, since in may contain blanks
!
! -------------- Read the next line, since no qualifiers is expected
!
                 LENGTH = CFREAD ( STRING )
!
                 LIM=I_LEN(BUF_STR)
                 IF ( LIM .EQ. 0 ) THEN
                      CALL FERR ( INT2(3062), &
     &                    'gflags: No value follows qualiter '//'USER_BUFFER', &
     &                     INT2(0), INT2(0) )
                      STOP 'BATCH(gflags) Abnormal termination'
                 END IF
!
                 DO I=1,LIM
                    IF ( BUF_STR(I:I) .NE. ' ' ) THEN
                         DELIMIT=BUF_STR(I:I)
                         DO J=I+1,LIM
                            IF ( BUF_STR(J:J) .EQ. DELIMIT ) THEN
                                 IF ( I+1 .GT. J-1 ) THEN
                                      CALL FERR ( INT2(3064), &
     &                                    'BATCH(gflags) empty '//'USER_BUFFER', &
     &                                     INT2(0), INT2(0) )
                                   ELSE IF( J-I-1 .GT. 80 ) THEN
                                      CALL FERR ( INT2(3066), &
     &                                    'BATCH(gflags) '// &
     &                                    'user_buffer too long: longer '// &
     &                                    'than 80 characters', INT2(0), INT2(0) )
                                 ENDIF
!
                                 USER_BUFF(1:)='Y'//BUF_STR(I+1:J-1)
                                 BUF_STR(I:J)=' '
                                 GOTO 100
                            ENDIF
                         ENDDO
                         WRITE ( 6, * ) 'Delimiter: ',DELIMIT
                         CALL FERR ( INT2(3067), 'GSETUP(BATCH) no closing '// &
     &                       'delimiter in USER_BUFFER '//BUF_STR(1:LIM), INT2(0), &
     &                        INT2(0) )
                    ENDIF
                 ENDDO
                 CALL FERR ( INT2(3068), 'GSETUP(BATCH) impossible conditon', &
     &                INT2(0), INT2(0) )
                 STOP 'BATCH(gflags) Abnormal termination'
               ELSE
                 CALL FERR ( INT2(3069), &
     &               'BATCH(gflags) Wrong qualifer after '// &
     &               'keyword USER_PROGRAM '//BUF_STR(1:I_LEN(BUF_STR))// &
     &               ' -- USER_BUFFER was expected', INT2(0), INT2(0) )
                 STOP 'BATCH(gflags) Abnormal termination'
            END IF
100         CONTINUE
          ELSE IF ( TOKEN .EQ. 'USER_PARTIALS' ) THEN
!
! --------- USER_PARTIALS
!
            CALL USE_GLBFIL ( 'OR' )
            CALL SPLITSTRING ( STRING, USER_PART_PROG, STRING )
            IF ( TRIMLEN(USER_PART_PROG) .LE. 0 ) THEN
                 CALL FERR ( INT2(3055), &
     &               'GSETUP(BATCH) no USER_PART_PROG specified', INT2(0), &
     &                INT2(0) )
            ENDIF
!
            IF ( USER_PART_PROG .EQ. 'NONE' .OR. USER_PART_PROG .EQ. 'NO' .OR. &
     &           USER_PART_PROG .EQ. 'none' .OR. USER_PART_PROG .EQ. 'no' ) THEN
                 CALL CLRCH ( USER_PART_PROG )
               ELSE
                 IF ( KUSPP ) THEN
                      OPEN ( 67, FILE=FILNAM, ACCESS="APPEND" )
                      WRITE ( 67, '(A)' ) USER_PART_PROG
                      CLOSE(67)
                      USER_PART_PROG=SOLVE_PROG_DIR//"CHPART"
                   ELSE
                      FILNAM = PRE_SCR_DIR(:PRE_SD_LEN)//'CPAR'//PRE_LETRS
                      OPEN  ( 67, FILE=FILNAM )
                      WRITE ( 67,'(A)' ) USER_PART_PROG
                      CLOSE ( 67 )
                ENDIF
                KUSER_PART = .TRUE.
            ENDIF
            KUSPP=.TRUE.
            CALL USE_GLBFIL ( 'WC' )
          ELSE IF ( TOKEN .EQ. 'USER_CONSTRAINTS' ) THEN
!
! --------- USER_CONSTRAINTS
!
            IF ( KUSCN ) CALL FERR ( INT2(3074), 'GSETUP(BATCH) Keyword '// &
     &          'USER_CONSTRAINTS used twice', INT2(0), INT2(0) )
            CALL USE_GLBFIL ( 'OR' )
            CALL SPLITSTRING ( STRING, USER_CONST_PROG, STRING )
            IF ( TRIMLEN ( USER_CONST_PROG) .LE. 0 ) THEN
                 CALL FERR ( INT2(3075), 'GSETUP(BATCH) no user_const_prog '// &
     &               'specified', INT2(0), INT2(0) )
            ENDIF
            IF (USER_CONST_PROG .EQ. 'NONE' .OR. USER_CONST_PROG .EQ. 'NO' .OR. &
     &          USER_CONST_PROG .EQ. 'none' .OR. USER_CONST_PROG .EQ. 'no') THEN
                 CALL CLRCH ( USER_CONST_PROG )
               ELSE
                 KUSER_CONST = .TRUE.
            END IF
            KUSCN = .TRUE.
            CALL USE_GLBFIL ( 'WC' )
          ELSE IF ( TOKEN .EQ. 'ELEVATION_DEPENDENT_NOISE' ) THEN
!
! --------- ELEVATION-DEPENDENT_NOISE
!
            IF ( KELDEP ) CALL FERR ( INT2(3076), &
     &        'ELEVATION_DEPENDENT_NOISE USED TWICE', INT2(0), INT2(0) )
            CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
            IF ( TOKEN .EQ. 'NO' ) THEN
              ELSE IF ( TOKEN .EQ. 'MULTI_GLOBAL' ) THEN
                 RWT_EL_USE = SOLVE__RW_EL_MULT_GLOB 
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( ILEN(TOKEN) == 0 ) THEN
                      CALL ERR_LOG ( 8663, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing ELEVATION_DEPENDENT_NOISE keyword: '// &
     &                    'a real value was expected after qualifier '// &
     &                    'MULT_GLOBAL, but nothing was supplied' )
                      RETURN 
                 END IF
!
                 READ ( UNIT=TOKEN, FMT='(F12.6)', IOSTAT=IER ) RWT_EL_GLB
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8664, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing ELEVATION_DEPENDENT_NOISE keyword: '// &
     &                    'a real value was expected after qualifier '// &
     &                    'MULT_GLOBAL, but '//TOKEN(1:I_LEN(TOKEN))// &
     &                    'was supplied' )
                      RETURN 
                 END IF
              ELSE IF ( TOKEN .EQ. 'GLOBAL' ) THEN
                 RWT_EL_USE = SOLVE__YES 
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( ILEN(TOKEN) == 0 ) THEN
                      CALL ERR_LOG ( 8665, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing ELEVATION_DEPENDENT_NOISE keyword: '// &
     &                    'a real value was expected after qualifier '// &
     &                    'GLOBAL, but nothing was supplied' )
                      RETURN 
                 END IF
!
                 READ ( UNIT=TOKEN, FMT='(F12.6)', IOSTAT=IER ) RWT_EL_GLB
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8666, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing ELEVATION_DEPENDENT_NOISE keyword: '// &
     &                    'a real value was expected after qualifier '// &
     &                    'GLOBAL, but '//TOKEN(1:I_LEN(TOKEN))// &
     &                    'was supplied' )
                      RETURN 
                 END IF
                 CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                 IF ( TOKEN == 'STATION_FILE' ) THEN
                      CALL SPLITSTRING ( STRING, RWT_STA_EL_FIL, STRING )
                      IF ( ILEN(TOKEN) == 0 ) THEN
                           CALL ERR_LOG ( 8667, IUER, 'GSETUP', 'Error in '// &
     &                         'parsing ELEVATION_DEPENDENT_NOISE keyword: '// &
     &                         'a file name was expected after qualifier '// &
     &                         'STATION_FILE, but nothing was supplied' )
                           RETURN 
                      END IF
                      CALL ERR_PASS ( IUER, IER )
                      CALL PARSE_ELDEP_FILE ( RWT_STA_EL_FIL, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8668, IUER, 'GSETUP', 'Error in '// &
     &                         'parsing ELEVATION_DEPENDENT_NOISE keyword: '// &
     &                         'an attempt to read station- and elevation- '// &
     &                         'dependent weights from file '// &
     &                          RWT_STA_EL_FIL(1:I_LEN(RWT_STA_EL_FIL))// &
     &                         'failed' )
                           RETURN 
                      END IF
                    ELSE IF ( ILEN(TOKEN) == 0 ) THEN
                      CONTINUE 
                    ELSE 
                      CALL ERR_LOG ( 8669, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing ELEVATION_DEPENDENT_NOISE keyword: '// &
     &                    'an unsupported qualifier '//TOKEN(1:I_LEN(TOKEN))// &
     &                    ' was found' )
                      RETURN 
                 END IF
              ELSE IF ( TOKEN .EQ. 'YES' ) THEN
                 KELDEP_NOISE = .TRUE.
                 CALL SPLITSTRING ( STRING, ELDEP_FILE, STRING )
            ENDIF
            KELDEP=.TRUE.
            CALL USE_GLBFIL_4 ( 'OWC' )
          ELSE IF ( TOKEN .EQ. 'ATMOSPHERE_STOCHASTIC_MODEL' ) THEN
!
! --------- ATMOSPHERE_STOCHASTIC_MODEL
!
            IF ( KASM ) CALL FERR ( INT2(3088), &
     &        'ATMOSPHERE_STOCHASTIC_MODEL used twice', INT2(0), INT2(0) )
            CALL SPLITSTRING ( STRING, TOKEN, STRING )
            IF ( TOKEN .EQ. 'NO' .OR. TOKEN .EQ. 'no' .OR. TOKEN .EQ. 'No' ) THEN
                 CONTINUE 
               ELSE 
                 ASM_NAM = TOKEN
                 IF ( ASM_NAM == "NILSON_2010" .OR. ASM_NAM == "HALSIG_2016" ) THEN
                      CALL ERR_LOG ( 8670, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing ATMOSPHERE_STOCHASTIC_MODEL keyword: '// &
     &                    'unsupported model '//TRIM(TOKEN)//' -- only '// &
     &                    'NILSSON_2010 and HALSIG_2016 are supported' )
                      RETURN 
                 END IF
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN == 'STA_PAR' ) THEN
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
                      ASM_STA_FIL = TOKEN
                      INQUIRE ( FILE=ASM_STA_FIL, EXIST=LEX )
                      IF ( .NOT. LEX ) THEN
                           CALL ERR_LOG ( 8671, IUER, 'GSETUP', 'Error in '// &
     &                         'parsing ATMOSPHERE_STOCHASTIC_MODEL keyword: '// &
     &                         'file '//TRIM(ASM_STA_FIL)//' does not exist' )
                           RETURN 
                      END IF
                    ELSE 
                      CALL ERR_LOG ( 8672, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing ATMOSPHERE_STOCHASTIC_MODEL keyword: '// &
     &                    'unsupported token '//TRIM(TOKEN)//' while STA_PAR '// &
     &                    'was expected' )
                      RETURN 
                 END IF
            END IF
          ELSE IF ( TOKEN .EQ. 'GPS_TEC_NOISE' ) THEN
            IF ( KTEC ) CALL FERR ( INT2(3079), 'GPS_TEC_NOISE USED TWICE', &
     &                              INT2(0), INT2(0) )
            CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
            IF ( TOKEN .EQ. '\' ) CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
            IF ( TOKEN .EQ. 'NO' ) THEN
                 CONTINUE 
              ELSE IF ( TOKEN .EQ. 'YES' ) THEN
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( ILEN(TOKEN) == 0 ) THEN
                      CALL ERR_LOG ( 8673, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing GPS_TEC_NOISE keyword: '// &
     &                    'a file name after the qualifier YES was '// &
     &                    'expected, but nothing was supplied' )
                      RETURN 
                 END IF
                 TEC_NOISE_FILE = TOKEN
!
                 CALL ERR_PASS  ( IUER, IER ) 
                 CALL PARSE_TCN ( TEC_NOISE_FILE, M__TCN, TCN, L_TCN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8674, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing file '//TEC_NOISE_FILE(1:I_LEN(TEC_NOISE_FILE))// &
     &                    'specified in the GPS_TEC_NOISE keyword ' )
                      RETURN 
                 END IF
                 CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. '\' ) CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'NO_IONO_CONTRIB' ) THEN
                      FL_NO_IONO_DEL = .TRUE.
                 ENDIF
            ENDIF
            KTEC = .TRUE.
            CALL USE_GLBFIL_4 ( 'OWC' )
          ELSE IF ( TOKEN .EQ. 'SNR_MIN' ) THEN
            IF ( KSNR ) CALL FERR ( INT2(3038), 'SNR_MIN USED TWICE', &
     &                              INT2(0), INT2(0) )
            CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
            IF ( TOKEN .EQ. '\' ) CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
            IF ( TOKEN .EQ. 'NO' ) THEN
                 CONTINUE 
              ELSE 
                 READ ( UNIT=TOKEN, FMT='(F10.2)', IOSTAT=IER ) GLO_SNR_MIN_X
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8675, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing SNR_MIN keyword: two real number were '// &
     &                    'expected, but the first number is '//TOKEN )
                      RETURN 
                 END IF
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( ILEN(TOKEN) == 0 ) THEN
                      CALL ERR_LOG ( 8676, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing SNR_MIN keyword: '// &
     &                    'two real numbers should follow, but only one '// &
     &                    'was spectied' )
                      RETURN 
                 END IF
                 READ ( UNIT=TOKEN, FMT='(F10.2)', IOSTAT=IER ) GLO_SNR_MIN_S
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8677, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing SNR_MIN keyword: two real number were '// &
     &                    'expected, but the second number is '//TOKEN )
                      RETURN 
                 END IF
            ENDIF
            KSNR = .TRUE.
            CALL USE_GLBFIL_4 ( 'OWC' )
          ELSE IF ( TOKEN .EQ. 'USER_TAG' ) THEN
!
! --------- USER_TAG
!
            IF ( KUSTAG) CALL FERR ( INT2(3033), 'USER_TAG USED TWICE', &
     &                               INT2(0), INT2(0) )
            CALL SPLITSTRING ( STRING, USER_TAG, STRING )
            CALL CASEFOLD(USER_TAG )
            KUSTAG = .TRUE.
            SOLARCH_SOL = .TRUE.
          ELSE IF ( TOKEN .EQ. 'SOL_TAG' ) THEN
!
! --------- SOL_TAG
!
            IF ( KSLTAG ) CALL FERR ( INT2(3034), 'SOL_TAG USED TWICE', &
     &                                INT2(0), INT2(0) )
            CALL SPLITSTRING ( STRING, SOL_TAG, STRING )
            CALL CASEFOLD ( SOL_TAG )
            KSLTAG=.TRUE.
            SOLARCH_SOL = .TRUE.
          ELSE IF ( TOKEN .EQ. 'FAST_MODE' ) THEN
!
! --------- FAST_MODE keyword
!
            IF ( KFAST_MODE ) CALL FERR ( INT2(3110), 'Keyword FAST_MODE '// &
     &                                   'used twice', INT2(0), INT2(0) )
            CALL SPLIT_AND_CASEFOLD ( STRING, FAST_MODE_TAG, STRING )
!
! --------- Scanning array of acceptable abbreviations FM_ABR (from fast.i) and
! --------- setting up the value
!
            FAST_MODE = F__UND
            DO 410 J1=1,FM_VAR
               IF ( FAST_MODE_TAG(1:3) .EQ. FM_ABR(J1) ) FAST_MODE = FM_VAL(J1)
 410        CONTINUE
            IF ( FAST_MODE .EQ. F__UND ) THEN
                 CALL FERR ( INT2(3112), 'Unknown FAST_MODE type: '// &
     &                FAST_MODE_TAG(1:16), INT2(0), INT2(0) )
            END IF
            FAST_MODE_GLO = FAST_MODE
            KFAST_MODE = .TRUE.
            CALL USE_GLBFIL_4 ( 'OWC' )
          ELSE IF ( TOKEN .EQ. 'FAST_DBG' ) THEN
!
! --------- FAST_MODE keyword
!
            IF ( KFAST_DBG ) CALL FERR ( INT2(3120), &
     &          'Keyword FAST_DBG used '//'twice', INT2(0), INT2(0) )
            CALL SPLIT_AND_CASEFOLD ( STRING, FAST_DBG_TAG, STRING )
!
! --------- Scanning array of acceptable abbreviations FD_ABR (from fast.i) and
! --------- setting up the value
!
            FAST_DBG = F__UND
            DO 420 J2=1,FD_VAR
               IF ( FAST_DBG_TAG(1:3) .EQ. FD_ABR(J2) ) FAST_DBG = FD_VAL(J2)
 420        CONTINUE
            IF ( FAST_DBG .EQ. F__UND ) THEN
                 CALL FERR ( INT2(3122), 'Unknown FAST_DBG type: '// &
     &                FAST_DBG_TAG(1:16), INT2(0), INT2(0) )
            END IF
            FAST_DBG_GLO = FAST_DBG
            KFAST_DBG = .TRUE.
            CALL USE_GLBFIL_4 ( 'OWC' )
          ELSE IF ( TOKEN .EQ. 'FAST_COV' ) THEN
!
! --------- FAST_COV keyword
!
            IF ( KFAST_COV ) CALL FERR ( INT2(3130), &
     &          'Keyword FAST_COV used '//'twice', INT2(0), INT2(0) )
            CALL SPLIT_AND_CASEFOLD ( STRING, FAST_COV_TAG, STRING )
!
! --------- Scanning array of acceptable abbreviations FC_ABR (from fast.i) and
! --------- setting up the value
!
            FAST_COV = F__UND
            DO 430 J3=1,FC_VAR
               IF ( FAST_COV_TAG(1:3) .EQ. FC_ABR(J3) ) FAST_COV = FC_VAL(J3)
 430        CONTINUE
            IF ( FAST_COV .EQ. F__UND ) THEN
                 CALL FERR ( INT2(3132), 'Unknown FAST_COV type: '// &
     &                FAST_COV_TAG(1:16), INT2(0), INT2(0) )
            END IF
            FAST_COV_GLO = FAST_COV
            KFAST_COV = .TRUE.
            CALL USE_GLBFIL_4 ( 'OWC' )
          ELSE IF ( TOKEN .EQ. 'SOLVE_COMPAT' ) THEN
            CALL ERR_LOG ( 8678, IUER, 'GSETUP', 'Keyword '// &
     &          'SOLVE_COMPAT is not supported any more. Please use keyword '// &
     &          'EMULATION NO or EMULATION 9612' )
            RETURN 
          ELSE IF ( TOKEN .EQ. 'EMULATION' ) THEN
!
! --------- EMULATION keyword
!
            IF ( KSOLVE_EMULATION ) CALL FERR ( INT2(3140), &
     &          'Keyword EMULATION'//'used twice', INT2(0), INT2(0) )
            CALL SPLIT_AND_CASEFOLD ( STRING, SOLVE_EMULATION_TAG, STRING )
            IF ( SOLVE_EMULATION_TAG(1:1) .EQ. '0' ) THEN
                 SOLVE_EMULATION = 0
              ELSE IF ( SOLVE_EMULATION_TAG(1:2) .EQ. 'NO' ) THEN
                 SOLVE_EMULATION = 0
              ELSE IF ( SOLVE_EMULATION_TAG(1:4) .EQ. '9612' ) THEN
                 SOLVE_EMULATION = 9612
              ELSE
                 SOLVE_EMULATION = -1
                 CALL ERR_LOG ( 8679, IUER, 'GSETUP', 'Wrong '// &
     &               'EMULATION value: '//SOLVE_EMULATION_TAG(1:12)// &
     &               ' -- only values 0 or 9612 are acceptable' )
                 RETURN 
            END IF
            KSOLVE_EMULATION = .TRUE.
            CALL USE_GLBFIL_4 ( 'OWC' )
          ELSE IF ( TOKEN .EQ. 'CRES_COMPAT' ) THEN
            CALL ERR_LOG ( 8680, IUER, 'GSETUP', 'Keyword '// &
     &          'CRES_COMPAT is not supported any more. Please use keyword '// &
     &          'CRES_EMULATION in the $OUTOUT section, for example, '// &
     &          'CRES_EMULATION NO' )
            RETURN 
          ELSE IF ( TOKEN .EQ. 'SUPMET' ) THEN
!
! --------- SUPMET keyword
!
            IF ( KSUPMET ) CALL FERR ( INT2(3180), 'Keyword SUPMET '// &
     &          'used twice', INT2(0), INT2(0) )
            CALL SPLIT_AND_CASEFOLD ( STRING, SUPMET_TAG, STRING )
            DO 440 J4=SUPMET__FIRST, SUPMET__LAST
!
! ------------ Get ascii description
!
               CALL SUPMET_SHOW ( J4, STR )
!
! ------------ Extract there code itself
!
               IB = INDEX ( STR, '__' ) + 2
               IE = INDEX ( STR, ' '  ) - 1
               IF ( IE.GT.IB ) THEN
!
! ----------------- Search occurrence of the code
!
                    IF ( INDEX ( SUPMET_TAG, STR(IB:IE) ) .GT. 0 ) THEN
                         IF ( SUPMET_BAT .NE. SUPMET__UND ) THEN
                              CALL ERR_LOG ( 8681, IUER, 'GSETUP', &
     &                            'Ambiguous code of suppression method: '// &
     &                             SUPMET_TAG(1:12) )
                              RETURN 
                         END IF
                         SUPMET_BAT = J4
                    END IF
               END IF
 440        CONTINUE
            IF ( SUPMET_BAT .EQ. SUPMET__UND  .AND. &
     &           SUPMET_TAG(1:3) .NE. 'UND'          ) THEN
!
                 CALL ERR_LOG ( 8682, IUER, 'GSETUP', 'Wrong '// &
     &               'SUPMET value: '//SUPMET_TAG(1:12) )
                 RETURN 
            END IF
            KSUPMET = .TRUE.
            CALL USE_GLBFIL_4 ( 'OWC' )
          ELSE IF ( TOKEN .EQ. 'QUALCODE_LIMIT' ) THEN
!
! --------- QUALCODE_LIMIT keyword
!
            IF ( KQUALCODE_LIMIT ) CALL FERR ( INT2(3190), &
     &          'Keyword QUALCODE_LIMIT '//'used twice', INT2(0), INT2(0) )
            CALL SPLIT_AND_CASEFOLD ( STRING, QUALCODE_LIMIT_TAG, STRING )
            CALL CHIN ( QUALCODE_LIMIT_TAG, IP )
            IF ( IP .GE. 1  .AND.  IP .LE. 9 ) THEN
                 QUALCODE_GOOD_LIM = IP
              ELSE
                 CALL ERR_LOG ( 8683, IUER, 'GSETUP', 'Wrong '// &
     &               'QUALCODE_LIMIT value: '//QUALCODE_LIMIT_TAG(1:12)// &
     &               ' -- only values in range [1, 9] are acceptable' )
                 RETURN 
            END IF
            KQUALCODE_LIMIT = .TRUE.
            CALL USE_GLBFIL_4 ( 'OWC' )
          ELSE IF ( TOKEN .EQ. 'WEIGHTS' ) THEN
!
! --------- WEIGHTS
!
            IF ( KWAIT ) THEN
                 CALL FERR ( INT2(3150), 'GTSETUP Keyword WEIGHTS used twice', &
     &                INT2(0), INT2(0) )
            END IF
            CALL SPLITSTRING ( STRING, TOKEN, STRING )
!
            IF ( TOKEN .NE. 'DEFAULT'  .AND.  &
     &           TOKEN .NE. 'USE'      .AND.  &
     &           TOKEN .NE. 'MAKE'     .AND.  &
     &           TOKEN .NE. 'APPEND'   .AND.  &
     &           TOKEN .NE. 'REQUIRE'  .AND.  &
     &           TOKEN .NE. 'INSERT'   .AND.  &
     &           TOKEN .NE. 'IN'       .AND.  &
     &           TOKEN .NE. 'NO' ) THEN
                 CALL FERR ( INT2(3151), 'GSETUP: Illegal weights option: '// &
     &                TOKEN(1:16)//' one of NO, IN, USE, MAKE, '// &
     &               'APPEND, REQUIRE was expected', INT2(0), INT2(0) )
                 STOP 'BATCH(gsetup)  abnormal termination'
            ENDIF
            WEIGHTS= TOKEN(1:1)
            IF ( TOKEN == 'INSERT' ) WEIGHTS = 'T'
!
            IF ( WEIGHTS   .EQ. 'M '     .AND. &
     &           FAST_MODE .NE.  F__NONE .AND. &
     &           .NOT. KFAST_COV              ) THEN
                 CALL FERR ( INT2(3158), 'GSETUP: Keywords WEIGHTS '// &
     &               'should be specified after the line FAST_COV = SEG', INT2(0), &
     &                INT2(0) )
                 STOP 'BATCH(gsetup)  abnormal termination'
            END IF
!
            IF ( WEIGHTS .EQ. 'I' ) THEN
                 WEIGHTS = 'D'
               ELSE IF ( WEIGHTS .EQ. 'N' ) THEN
                 CONTINUE
               ELSE
                 CALL SPLITSTRING ( STRING, WEIGHT_FILE(1), STRING )
                 IF ( WEIGHT_FILE(1) == '\' ) THEN
                      LENGTH = CFREAD ( STRING )
                      CALL SPLITSTRING ( STRING, WEIGHT_FILE(1), STRING )
                 END IF
                 IF ( TRIMLEN(WEIGHT_FILE(1)) .LE. 0 ) THEN
                      CALL FERR ( INT2(3152), &
     &                    'GSETUP: no weight_file specified', INT2(0), INT2(0) )
                      STOP 'BATCH(gsetup)  abnormal termination'
                 ENDIF
                 CALL CHIN ( WEIGHT_FILE(1)(1:2), LF_WEI )
                 IF ( LF_WEI .GE. 1  .AND.  LF_WEI .LE. MF_WEI ) THEN
                      CALL SPLITSTRING ( STRING, WEIGHT_FILE(1), STRING )
                    ELSE
                      LF_WEI = 1
                 END IF
!
                 DO 450 J5=1,LF_WEI
                    IF ( WEIGHT_FILE(J5) == '\' ) THEN
                         LENGTH = CFREAD ( STRING )
                         CALL SPLITSTRING ( STRING, WEIGHT_FILE(J5), STRING )
                    END IF
                    IF ( WEIGHT_FILE(J5)(1:1) .NE. '/' ) THEN
                         WEIGHT_FILE(J5) = PRE_SAV_DIR(1:PRE_SV_LEN)// &
     &                                     WEIGHT_FILE(J5)
                    END IF
!
                    CALL SPLITSTRING ( STRING, TOKEN, STRING )
                    IF ( TOKEN == '\' ) THEN
                         LENGTH = CFREAD ( STRING )
                         CALL SPLITSTRING ( STRING, TOKEN, STRING )
                    END IF
                    IF ( J5 .NE. LF_WEI ) THEN
                         STRING_SAVE = STRING
                         WEIGHT_FILE(J5+1) =  TOKEN
                    END IF
 450             CONTINUE 
!
                 IF ( WEIGHTS .NE. 'D' ) THEN
                      IF ( WEIGHT_FILE(LF_WEI) .EQ. 'BY_SITE'      .OR. &
     &                     WEIGHT_FILE(LF_WEI) .EQ. 'BY_BASELINE'  .OR. &
     &                     WEIGHT_FILE(LF_WEI) .EQ. 'BY_ARC'            ) THEN
                           CALL FERR ( INT2(3165), &
     &                         'GSETUP: First specify a weight '// &
     &                         'file, then a type: BY_SITE, BY_BASELINE '// &
     &                         'or BY_ARC', INT2(0), INT2(0) )
                           STOP 'BATCH(GSETUP) Abnormal termination'
                      END IF
!
                      WEIGHT_PARSE = TOKEN
                      IF ( WEIGHT_PARSE .EQ. 'ALGORITHM' ) THEN
                           WEIGHT_PARSE = 'BY_ARC'
                           STRING = STRING_SAVE
                         ELSE IF ( TRIMLEN(WEIGHT_PARSE) .LE. 0 ) THEN
                           WEIGHT_PARSE = 'BY_ARC'
                      END IF
!
                      IF ( WEIGHT_PARSE .EQ. 'BY_SITE' ) THEN
                           WEIGHT_TYPE_MA = 'S'
                         ELSE IF ( WEIGHT_PARSE .EQ. 'BY_BASELINE' ) THEN
                           WEIGHT_TYPE_MA = 'B'
                         ELSE IF ( WEIGHT_PARSE .EQ. 'BY_ARC' ) THEN
                           WEIGHT_TYPE_MA = 'A'
                         ELSE
                           CALL FERR ( INT2(3066), 'GSETUP: wrong '// &
     &                         'weight type: '//WEIGHT_PARSE// &
     &                         ' Valid weight types are '// &
     &                         'BY_SITE, BY_BASELINE or BY_ARC', &
     &                          INT2(0), INT2(0) )
                           STOP 'BATCH(GSETUP) Abnormal termination'
                      END IF
!
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
!
                      IF ( TOKEN .EQ. 'ALGORITHM' ) THEN
                           IF ( WEIGHTS .NE. 'M'  .AND.  &
     &                          WEIGHTS .NE. 'A'  .AND.  &
     &                          WEIGHTS .NE. 'T'         ) THEN
                                CALL FERR ( INT2(3067), &
     &                              'GSETUP: token ALGORITHM '// &
     &                              'is supported only when weights are '// &
     &                              'being made', INT2(0), INT2(0) )
                                STOP 'BATCH(GSETUP) Abnormal termination'
                           END IF
!
! ------------------------ Get one token more to learn the ID of the algorithm
!
                           CALL SPLITSTRING ( STRING, TOKEN, STRING )
                           IF ( TOKEN .EQ. 'MYWAY' ) THEN
                                WEIGHT_ALGORITHM = WEIGHT__MYWAY
                              ELSE IF ( TOKEN .EQ. 'UPWEI' ) THEN
                                WEIGHT_ALGORITHM = WEIGHT__UPWEI
                              ELSE IF ( TOKEN .EQ. 'UPWEI_OPT' ) THEN
                                WEIGHT_ALGORITHM = WEIGHT__UPWEI_OPT
                              ELSE IF ( TOKEN .EQ. 'ELIM' ) THEN
                                WEIGHT_ALGORITHM = WEIGHT__ELIM
                                CALL SPLITSTRING ( STRING, PARU_FILE, STRING )
                                IF ( ILEN(PARU_FILE) == 0 ) THEN
                                     CALL FERR ( INT2(3194), &
     &                                   'GSETUP: no paru_file was '// &
     &                                   'supplied after qualifiers '// &
     &                                   'ALGORITHM ELIM of WEIGHT keyword', &
     &                                   INT2(0), INT2(0) )
                                     STOP 'BATCH(GSETUP) Abnormal termination'
                                END IF
                              ELSE IF ( TOKEN(1:1) .EQ. ' ' ) THEN
                                CALL FERR ( INT2(3067), &
     &                              'GSETUP: no value is '// &
     &                              'supplied after qualifier '// &
     &                              'ALGORITHM of WEIGHT keyword', INT2(0), &
     &                               INT2(0) )
                                STOP 'BATCH(GSETUP) Abnormal termination'
                              ELSE
                                CALL FERR ( INT2(3068), &
     &                              'GSETUP: unsupported value '// &
     &                              'of the qualifier ALGORITHM of WEIGHT '// &
     &                              'keyword: '//TOKEN(1:16)//' one of '// &
     &                              'MYWAY, UPWEI, UPWEI_OPT  was expected', &
     &                               INT2(0), INT2(0) )
                                STOP 'BATCH(GSETUP) Abnormal termination'
                           END IF ! Token
                         ELSE IF ( TOKEN(1:1) .EQ. ' ' ) THEN
!
! ------------------------ Algortihm has not been specified explicitely
! ------------------------ Set default
!
                           IF ( WEIGHTS .EQ. 'M'  .OR.  WEIGHTS .EQ. 'A' ) THEN
                                WEIGHT_ALGORITHM = WEIGHT__MYWAY
                           END IF
                         ELSE
                           CALL FERR ( INT2(3069), &
     &                         'GSETUP: invalid token for '// &
     &                         'WEIGHT keyword: ALGORITHM was expected', INT2(0), &
     &                          INT2(0) )
                           STOP 'BATCH(GSETUP) Abnormal termination'
                      END IF ! token ALGORITHM
                 END IF ! token WEIGHT
!
                 IF ( ( WEIGHT_ALGORITHM .EQ. WEIGHT__UPWEI .OR. &
     &                  WEIGHT_ALGORITHM .EQ. WEIGHT__UPWEI_OPT ) .AND. &
     &                  WEIGHT_TYPE_MA .NE. 'B' ) THEN
                        CALL FERR ( INT2(3070), &
     &                      'GSETUP: Weight algorithm UPWEI '// &
     &                      'doesn''t support weith type '//WEIGHT_PARSE// &
     &                      ' -- only BY_BASELINE', INT2(0), INT2(0) )
                        STOP 'BATCH(GSETUP) Abnormal termination'
                 END IF
            ENDIF
!
            KWAIT=.TRUE.
            IF ( ( WEIGHTS == 'A'  .OR.  WEIGHTS == 'I'  .OR.  &
     &             WEIGHTS == 'M' )  .AND.  LF_WEI > 1         ) THEN
                 CALL FERR ( INT2(3071), 'GSETUP: More than one output '// &
     &               'file name is specified in the mode of weights '// &
     &               'computation. This is wrong. Please specify only '// &
     &               'one name -- the namwe which you like the most', &
     &               INT2(0), INT2(0) )
            END IF
          ELSE IF ( TOKEN .EQ. 'SOURCE_WEIGHTS' ) THEN
!
! --------- SOURCE_WEIGHT_FILE
!
            IF ( KSWAIT ) THEN
                 CALL FERR ( INT2(3050), 'GSETUP(BATCH) Keyword '// &
     &               'SOURCE_WEIGHTS USED twice', INT2(0), INT2(0) )
            END IF
            CALL SPLITSTRING ( STRING, TOKEN, STRING )
            IF ( TOKEN .NE. 'USE' .AND. TOKEN .NE. 'REQUIRE'  .AND. &
     &           TOKEN .NE. 'NO' ) THEN
                 CALL FERR ( INT2(3051), 'GSETUP(BATCH) Illegal qualifier '// &
     &               'after SOURCE_WEIGHTS keyword: '//TOKEN(1:16)// &
     &               ' . One of NO, USE or REQUIRE was expected', INT2(0), &
     &                INT2(0) )
            ENDIF
!
            IF ( TOKEN .EQ. 'NO' ) THEN
                 CONTINUE
               ELSE
                 SOURCE_WEIGHTS = TOKEN(1:2)
                 CALL SPLITSTRING ( STRING, SOURCE_WEIGHT_FILE, STRING )
                 IF ( TRIMLEN(SOURCE_WEIGHT_FILE) .LE. 0 ) THEN
                      call ferr ( INT2(3052), &
     &                    'GSETUP(BATCH) No source_weight_file '//'specified', &
     &                     INT2(0), INT2(0) )
                 ENDIF
                 CALL USE_GLBFIL_4 ( 'OWC' )
            END IF
            KSWAIT = .TRUE.
          ELSE IF ( TOKEN .EQ. 'SAVING_RATE' ) THEN
!
! --------- SAVING_RATE keyword
!
            IF ( KSAVING_RATE ) CALL FERR ( INT2(3150), &
     &          'Keyword SAVING_RATE '//'used twice', INT2(0), INT2(0) )
            CALL SPLIT_AND_CASEFOLD ( STRING, SAVING_RATE_TAG, STRING )
!
            CALL CHIN ( SAVING_RATE_TAG, SAVING_RATE )
            IF ( SAVING_RATE .LE. 0  .OR.  SAVING_RATE .GT. 1000000 ) THEN
                 SAVING_RATE = 1
                 CALL ERR_LOG ( 8684, IUER, 'GSETUP', 'Wrong '// &
     &               'SAVING_RATE value: '//SAVING_RATE_TAG(1:12)// &
     &               ' -- integer values in the range [1, 1000000 ] '// &
     &               'are supported' )
                 RETURN 
            END IF
            KSAVING_RATE = .TRUE.
            CALL USE_GLBFIL_4 ( 'OWC' )
          ELSE IF ( TOKEN .EQ. 'SINGULARITY_CHECK' ) THEN
!
! --------- SINGULARITY_CHECK
!
            IF ( KSNGCHK ) CALL FERR ( INT2(3160), &
     &          'Keyword SINGULARITY_CHECK '//'used twice', INT2(0), INT2(0) )
            CALL ERR_PASS( IUER, IER )
            CALL SNGCHK_PARSER ( STRING, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8685, IUER, 'GSETUP', 'Error during '// &
     &               'parsing keyword SINGULARITY_CHECK' )
                 RETURN 
            END IF
            KSNGCHK = .TRUE.
            CALL USE_GLBFIL_4 ( 'OWC' )
          ELSE IF ( TOKEN .EQ. 'TRAIN' ) THEN
!
! --------- TRAIN
!
            IF ( KTRAIN ) CALL FERR ( INT2(3170), 'Keyword TRAIN '// &
     &          'used twice', INT2(0), INT2(0) )
            CALL ERR_PASS( IUER, IER )
            CALL TRAIN_PARSER ( STRING, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8686, IUER, 'GSETUP', 'Error during '// &
     &               'parsing keyword TRAIN' )
                 RETURN 
            END IF
            KTRAIN = .TRUE.
            CALL USE_GLBFIL_4 ( 'OWC' )
          ELSE IF ( TOKEN .EQ. 'SORT' ) THEN
!
! --------- SORT
!
            IF ( KSORT ) CALL FERR ( INT2(3180), 'Keyword SORT '// &
     &          'used twice', INT2(0), INT2(0) )
            CALL ERR_PASS( IUER, IER )
            CALL SORT_PARSER ( STRING, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8687, IUER, 'GSETUP', 'Error during '// &
     &               'parsing keyword SORT' )
                 RETURN 
            END IF
            KSORT = .TRUE.
            CALL USE_GLBFIL_4 ( 'OWC' )
          ELSE IF ( TOKEN .EQ. 'WARNING' ) THEN
!
! --------- WARNING
!
            IF ( KWARNING ) CALL FERR ( INT2(3190), 'Keyword WARNING '// &
     &          'used twice', INT2(0), INT2(0) )
            CALL SPLIT_AND_CASEFOLD ( STRING, WARNING_TAG, STRING )
            IF ( WARNING_TAG .EQ. 'ON' ) THEN
                 G_WARNING = .TRUE.
               ELSE IF ( WARNING_TAG .EQ. 'OFF' ) THEN
                 G_WARNING = .FALSE.
               ELSE IF ( WARNING_TAG .EQ. 'NO' ) THEN
                 G_WARNING = .FALSE.
               ELSE
                 CALL FERR ( INT2(3192), 'Wrong qualifier '//WARNING_TAG// &
     &               ' was found in parsing keyword WARNING. One of'// &
     &               ' ON, OFF, NO was expected', INT2(0), INT2(0) )
                 STOP 'BATCH(gsetup)  abnormal termination'
            END IF
            CALL USE_GLBFIL_4 ( 'OWC' )
          ELSE IF ( TOKEN .EQ. 'DECIMATION' ) THEN
!
! --------- Parse DECIMATION keyword
!
            IF ( KEDC ) CALL FERR ( INT2(3193), 'Keyword DECIMATION '// &
     &          'used twice', INT2(0), INT2(0) )
            CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
            IF ( TOKEN == 'NO' .OR. TOKEN == 'NONE' ) THEN
                 CALL CLRCH ( EDC_DIR )
                 EDC_USE  = EDC__UNDF
                 EDC_PAR  = 0
               ELSE IF ( TOKEN == 'CREATE' ) THEN
                 EDC_USE = EDC__CRE
!
! -------------- Get directory name
!
                 CALL SPLIT_STRING ( STRING, EDC_DIR, STRING )
!
! -------------- Check whether directory exists
!
                 DIR_DESC = OPENDIR ( EDC_DIR(1:I_LEN(EDC_DIR))//CHAR(0) )
                 IF ( DIR_DESC .LE. 0 ) THEN
                      CALL GERROR ( STR )
                      CALL ERR_LOG ( 8688, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing keyword DECIMATION in the $SETUP '// &
     &                    'section of the batch control file: cannot '// &
     &                    'open directory '//EDC_DIR(1:I_LEN(EDC_DIR))// &
     &                    ' --- '//STR )
                      RETURN 
                 END IF 
                 CALL CLOSEDIR ( %VAL(DIR_DESC) )
                 CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                 IF ( TOKEN == 'ASCII' ) THEN
                      EDC_PAR = EDC__ASC
                    ELSE IF ( TOKEN == 'BINARY' ) THEN
                      EDC_PAR = EDC__BIN
                    ELSE IF ( TOKEN == '' ) THEN
                      CALL ERR_LOG ( 8689, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing keyword DECIMATION in the $SETUP '// &
     &                    'section of the batch control file: the third '// &
     &                    'value was not specified. It should be ASCII or '// &
     &                    'BINARY' )
                      RETURN 
                    ELSE 
                      CALL ERR_LOG ( 8690, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing keyword DECIMATION in the $SETUP '// &
     &                    'section of the batch control file: the third '// &
     &                    'value, '//STR(1:I_LEN(STR))//' is not '// &
     &                    'recognized. ASCII or BINARY was expected' )
                      RETURN 
                 END IF
               ELSE IF ( TOKEN == 'USE' .OR. TOKEN == 'REQUIRE' ) THEN
                 IF ( TOKEN == 'USE'     ) EDC_USE = EDC__USE
                 IF ( TOKEN == 'REQUIRE' ) EDC_USE = EDC__REQ
!
! -------------- Get directory name
!
                 CALL SPLIT_STRING ( STRING, EDC_DIR, STRING )
!
! -------------- Check whether direcotry exists
!
                 DIR_DESC = OPENDIR ( EDC_DIR(1:I_LEN(EDC_DIR))//CHAR(0) )
                 IF ( DIR_DESC .LE. 0 ) THEN
                      CALL GERROR ( STR )
                      CALL ERR_LOG ( 8691, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing keyword DECIMATION in the $SETUP '// &
     &                    'section of the batch control file: cannot '// &
     &                    'open directory '//EDC_DIR(1:I_LEN(EDC_DIR))// &
     &                    ' --- '//STR )
                      RETURN 
                 END IF 
                 CALL CLOSEDIR ( %VAL(DIR_DESC) )
!
! -------------- Get parameter
!
                 CALL SPLIT_STRING ( STRING, STR, STRING )
                 IF ( ILEN(STR) == 0 ) THEN
                      CALL ERR_LOG ( 8692, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing keyword DECIMATION in the $SETUP '// &
     &                    'section of the batch control file: the third '// &
     &                    'value was not specified' )
                      RETURN 
                 END IF
!
                 READ ( UNIT=STR, FMT='(I8)', IOSTAT=IER ) EDC_PAR
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8693, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing keyword DECIMATION in the $SETUP '// &
     &                    'section of the batch control file: the third '// &
     &                    'value, '//STR(1:I_LEN(STR))//' , is not integer' )
                      RETURN 
                 END IF
               ELSE
                 CALL ERR_LOG ( 8694, IUER, 'GSETUP', 'Error in '// &
     &               'parsing keyword DECIMATION in the $SETUP '// &
     &               'section of the batch control file: invalid first '// &
     &               'value, '//TOKEN(1:I_LEN(TOKEN))//' -- one of '// &
     &               'CREATE, USE, or REQUIRE were expected' )
                 RETURN 
            END IF
            CALL USE_GLBFIL_4 ( 'OWC' )
            KEDC = .TRUE.
          ELSE IF ( TOKEN .EQ. 'THEORETICAL_DELAY_FILE' ) THEN
            IF ( KTPD ) CALL FERR ( INT2(3194), 'Keyword THEORETICAL_DELAY_FILE '// &
     &          'used twice', INT2(0), INT2(0) )
            CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
            IF ( TOKEN == 'DATABASE' ) THEN
                 TPD_FLAG = TPD__DBS
               ELSE IF ( TOKEN == 'IGNORE'   ) THEN
                 TPD_FLAG = TPD__IGN
               ELSE IF ( TOKEN == 'USE'      ) THEN
                 TPD_FLAG = TPD__USE
               ELSE IF ( TOKEN == 'UPDATE'   ) THEN
                 TPD_FLAG = TPD__UPD
               ELSE 
                 CALL ERR_LOG ( 8695, IUER, 'GSETUP', 'Error in '// &
     &               'parsing keyword THEORETICAL_DELAY_FILE in the $SETUP '// &
     &               'section of the batch control file: invalid '// &
     &               'value, '//TOKEN(1:I_LEN(TOKEN))//' -- one of '// &
     &               'DATABASE, IGNORE, USE, or UPDATE were expected' )
                 RETURN 
            END IF
            IF ( TPD_FLAG == TPD__USE  .OR.  TPD_FLAG == TPD__UPD ) THEN
                 CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                 IF ( ILEN(TOKEN) == 0 ) THEN
                      CALL ERR_LOG ( 8696, IUER, 'GSETUP', 'Error in '// &
     &                   'parsing keyword THEORETICAL_DELAY_FILE in the '// &
     &                    '$SETUP ection of the batch control file: no '// &
     &                    'DIRECTORY qualifier was suppled' )
                      RETURN 
                   ELSE IF ( TOKEN == 'DIRECTORY' ) THEN
                      CONTINUE 
                   ELSE 
                      CALL ERR_LOG ( 8697, IUER, 'GSETUP', 'Error in '// &
     &                   'parsing keyword THEORETICAL_DELAY_FILE in the '// &
     &                    '$SETUP ection of the batch control file: found '// &
     &                    'qualiier '//TOKEN(1:I_LEN(TOKEN))//' while '// &
     &                    'DIRECTORY was expected' )
                      RETURN 
                 END IF
                 CALL SPLIT_STRING ( STRING, TPD_DIR, STRING )
                 IF ( ILEN(TPD_DIR) == 0 ) THEN
                     CALL ERR_LOG ( 8698, IUER, 'GSETUP', 'Error in '// &
     &                   'parsing keyword THEORETICAL_DELAY_FILE in the '// &
     &                    '$SETUP ection of the batch control file: the '// &
     &                    'second value was not specified. It should be '// &
     &                    'directory name' )
                      RETURN 
                 END IF
                 IF ( TPD_DIR(I_LEN(TPD_DIR):I_LEN(TPD_DIR)) == '/' ) THEN
                      TPD_DIR(I_LEN(TPD_DIR):I_LEN(TPD_DIR)) = ' '
                 END IF
!
! -------------- Check whether directory exists
!
                 DIR_DESC = OPENDIR ( TPD_DIR(1:I_LEN(TPD_DIR))//CHAR(0) )
                 IF ( DIR_DESC .LE. 0 ) THEN
                      CALL GERROR ( STR )
                      CALL ERR_LOG ( 8699, IUER, 'GSETUP', 'Error in '// &
     &                    'parsing keyword THEORETICAL_DELAY_FILE in the '// &
     &                    '$SETUP section of the batch control file: cannot '// &
     &                    'open directory '//TPD_DIR(1:I_LEN(TPD_DIR))// &
     &                    ' --- '//STR )
                      RETURN 
                 END IF 
!
                 CALL CLOSEDIR ( %VAL(DIR_DESC) )
!
                 CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                 IF ( ILEN(TOKEN) == 0 ) THEN
                      CONTINUE 
                   ELSE IF ( TOKEN == 'USER_INIT' ) THEN
                      DO 460 J6=1,M__TPD_INIT
                         CALL SPLIT_AND_CASEFOLD ( STRING, TPD_INIT_LIST(J6), &
     &                                             STRING )
                         IF ( ILEN(TPD_INIT_LIST) > 0 ) THEN
                              N_TPD_INIT = N_TPD_INIT + 1
                            ELSE 
                              GOTO 860
                         END IF
 460                  CONTINUE 
 860                  CONTINUE 
                   ELSE 
                     CALL ERR_LOG ( 8700, IUER, 'GSETUP', 'Error in '// &
     &                   'parsing keyword THEORETICAL_DELAY_FILE in the '// &
     &                    '$SETUP ection of the batch control file: found '// &
     &                    'qualiier '//TOKEN(1:I_LEN(TOKEN))//' while '// &
     &                    'USER_INIT was expected' )
                      RETURN 
                 END IF
            END IF
            CALL USE_GLBFIL_4 ( 'OWC' )
            KTPD = .TRUE.
          ELSE IF ( TOKEN .EQ. 'EXTERNAL_IONO_PATH_DELAY' ) THEN
            IF ( KION ) THEN
                 CALL FERR ( INT2(3197), 'Keyword EXTERNAL_IONO_PATH_DELAY '// &
     &                      'used twice', INT2(0), INT2(0) )
                 STOP 'BATCH gsetup abnormal ternimation' 
            END IF
            CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
            IF ( TOKEN == 'NO' .OR. TOKEN == 'NONE' ) THEN
                 CALL CLRCH ( IONOV_DIR )
                 IONOV_USE = IONOV__NONE
               ELSE
                 IF ( TOKEN == 'GEN' ) THEN
                      IONOV_USE = IONOV__GEN
                    ELSE IF ( TOKEN == 'LOAD' ) THEN
                      IONOV_USE = IONOV__LOAD
                    ELSE IF ( TOKEN == 'USE' ) THEN
                      IONOV_USE = IONOV__USE
                    ELSE 
                      CALL ERR_LOG ( 8703, IUER, 'GSETUP', 'Wrong 1st '// &
     &                    'qualifier of the EXTERNAL_IONO_PATH_DELAY keyword: '// &
     &                     TOKEN(1:I_LEN(TOKEN))//' while GEN or '// &
     &                    'LOAD or USE were expected' )
                      RETURN 
                 END IF
!
                 CALL SPLIT_STRING ( STRING, IONOV_DIR, STRING )
            END IF
            CALL USE_GLBFIL_4 ( 'OWC' )
            KION = .TRUE.
          ELSE IF ( TOKEN .EQ. 'DTEC_USE' ) THEN
            IF ( KDTEC ) THEN
                 CALL FERR ( INT2(3198), 'Keyword DTEC_USE '// &
     &                      'used twice', INT2(0), INT2(0) )
                 STOP 'BATCH gsetup abnormal ternimation' 
            END IF
            CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
            IF ( TOKEN == 'NO' .OR. TOKEN == 'NONE' ) THEN
                 DTEC_USE = DTEC__NONE
               ELSE
                 IF ( TOKEN == 'APPLY' ) THEN
                      DTEC_USE = DTEC__APPLY
                    ELSE IF ( TOKEN == 'IMPORT' ) THEN
                      DTEC_USE = DTEC__IMPORT
                    ELSE 
                      CALL ERR_LOG ( 8703, IUER, 'GSETUP', 'Wrong 1st '// &
     &                    'qualifier of the DTEC keyword: '// &
     &                     TOKEN(1:I_LEN(TOKEN))//' while APPLY or IMPORT '// &
     &                    'or NONE were expected' )
                      RETURN 
                 END IF
!
                 CALL SPLIT_STRING ( STRING, IONOV_DIR, STRING )
            END IF
            CALL USE_GLBFIL_4 ( 'OWC' )
            KDTEC = .TRUE.
          ELSE IF ( INDEX ( TOKEN, 'DEFINE' ) > 0 ) THEN
            CALL CLRCH ( STRING ) ! This is needed to stop parsing tokens
          ELSE IF ( INDEX ( TOKEN, 'UNDEFINE' ) > 0 ) THEN
            CALL CLRCH ( STRING ) ! This is needed to stop parsing tokens
          ELSE
!
! --------- Something that isn't suppose to be there
!
            CALL FERR ( INT2(3090), '(GSETUP) Invalid keyword '//TOKEN(1:32), &
     &                  INT2(0), INT2(0) )
          ENDIF
        ENDDO
        LENGTH=CFREAD(STRING)
      ENDDO
!
! --- Test of conflicting options (global mode supports only part of
! --- fast modes)
!
      IF ( SOLTYP .NE. 'I'  .AND. &
     &     ( FAST_MODE .NE. F__NONE .AND. FAST_MODE .NE. F__B1B3D ) ) THEN
           CALL CLRCH ( STR )
           IF ( SOLTYP .EQ. 'F' .AND. .NOT. SUBTRACT_ARC ) STR = 'FORWARD'
           IF ( SOLTYP .EQ. 'B' ) STR = 'BACK'
           IF ( SOLTYP .EQ. 'S' ) STR = 'SUPPRESSION'
           IF ( SOLTYP .EQ. 'C' ) STR = 'COMPLETE'
           IF ( SOLTYP .EQ. 'G' ) STR = 'GLOBAL_ONLY'
           IF ( SOLTYP .EQ. 'F' .AND. SUBTRACT_ARC ) STR = 'SUBTRACT'
           IP = IFIND_PL ( FM_VAR, FM_VAL, FAST_MODE )
           IF ( IP .LE. 0 ) IP = 1
           CALL ERR_LOG ( 8704, IUER, 'GSETUP', 'Conflicting '// &
     &         'options in SETUP section of batch file: SOLUTION '// &
     &          STR(1:I_LEN(STR))//' and FAST_MODE is '//FM_STR(IP)(1:FM_LEN(IP))// &
     &         ' Only fast modes B1B3D or '// &
     &         'NONE are supported for the global solutions. Please '// &
     &         'change or add the line in SETUP section of your batch '// &
     &         'file: FAST_MODE B1B3D' )
           RETURN 
      END IF
!
      IF ( SOLTYP .EQ. 'I'  .AND. &
     &     ( FAST_MODE .NE. F__NONE  .AND.  FAST_MODE .NE. F__B3D  .AND. &
     &       FAST_MODE .NE. F__PRD ) ) THEN
           IP = IFIND_PL ( FM_VAR, FM_VAL, FAST_MODE )
           IF ( IP .LE. 0 ) IP = 1
           CALL ERR_LOG ( 8705, IUER, 'GSETUP', 'Conflicting '// &
     &         'options in SETUP section of batch file: SOLUTION '// &
     &         'INDEPENDENT and FAST_MODE is '//FM_STR(IP)(1:FM_LEN(IP))// &
     &         '. Only fast modes B3D or NONE are supported for the '// &
     &         'independent solutions. Please change or add the line in '// &
     &         'SETUP section of your batch file: FAST_MODE B3D' )
           RETURN 
      END IF
!
      IF ( FAST_MODE .NE. F__NONE  .AND.  SOLVE_EMULATION .NE. 0 ) THEN
           IP = IFIND_PL ( FM_VAR, FM_VAL, FAST_MODE )
           IF ( IP .LE. 0 ) IP = 1
           CALL CLRCH ( STR )
           CALL INCH  ( SOLVE_EMULATION, STR )
           CALL ERR_LOG ( 8706, IUER, 'GSETUP', 'Conflicting '// &
     &         'options in SETUP section of the control file: '//'EMULATION is '// &
     &          STR(1:I_LEN(STR))//' and FAST_MODE is '//FM_STR(IP)(1:FM_LEN(IP))// &
     &         ' . Compatibility mode with '// &
     &         'archaic version of SOLVE is supported only when FAST_MODE '// &
     &         'NONE ' )
           RETURN 
      END IF
!
      IF (     FAST_MODE .NE. F__B1B3D          .AND. &
     &            SOLTYP .NE. 'I'               .AND. &
     &     SNGCHK_ACTION .EQ. SNGCHK_ACT__REPR        ) THEN
!
           CALL ERR_LOG ( 8707, IUER, 'GSETUP', 'Conflicting '// &
     &         'options in SETUP section of the control file: '// &
     &         'SINGULARITY_CHECK ACTION  REPARAMETERIZE is valid only in '// &
     &         'B1B3D mode when global solution is obtained' )
           RETURN 
      END IF
!
      IF ( SUBTRACT_ARC .AND. .NOT. TRAIN ) THEN
           CALL ERR_LOG ( 8708, IUER, 'GSETUP', 'Conflicting '// &
     &         'options in SETUP section of the control file: '// &
     &         'NO TRAIN mode does not support SUBRACTION solution type. '// &
     &         'Please use TRAIN mode if you need subtract solution type' )
           RETURN 
      END IF
!
      IF ( .NOT. TRAIN                                   .AND. &
     &     ( WEIGHTS .EQ. 'A'  .OR.  WEIGHTS .EQ. 'M' )  .AND. &
     &       WEIGHT_ALGORITHM .EQ. WEIGHT__MYWAY               ) THEN
           CALL ERR_LOG ( 8709, IUER, 'GSETUP', 'Conflicting '// &
     &         'options in SETUP section of the control file: '// &
     &         'NO TRAIN mode does not support making weights files in '// &
     &         'WEIGHTS__MYWAY mode' )
           RETURN 
      END IF
!
      IF ( TRAIN .AND. SOLTYP .EQ. 'G' ) THEN
           CALL ERR_LOG ( 8710, IUER, 'GSETUP', 'Conflicting '// &
     &         'options in SETUP section of the control file: '// &
     &         'SOLUTION GLOBAL_ONLY is not compatible with TRAIN YES. '// &
     &         'Please use TRAIN NO mode' )
           RETURN 
      END IF
      CALL SET_MERG ( CMERG )
!
! --- Figure out what do now that we are done with this section:
!
! --- Make sure no major keywords are missing
!
      IF ( SOLARCH_SOL) THEN
           IF ( .NOT. KUSTAG  .OR. .NOT.KSLTAG  ) THEN
                CALL FERR ( INT2(3093), 'Need both user_tag and sol_tag to '// &
     &              'interface with solution archiving system', &
     &               INT2(0), INT2(0) )
           END IF
      END IF
!
!
      IF ( .NOT. KSOLUT  .AND. &
     &             KCGM  .AND. &
     &             KARC  .AND. &
     &              KID        ) THEN
           CALL FERR ( INT2(3095), 'GSETUP: Missing keywords', INT2(0), &
     &          INT2(0) )
      ENDIF
!
      IF ( FAST_MODE .NE. F__NONE  .AND.  .NOT. KFAST_COV ) THEN
           CALL FERR ( INT2(3198), 'Mandatory keyword FAST_COV missed in '// &
     &         '$SETUP section of the control file', INT2(0), INT2(0) )
           STOP 'BATCH(gsetup)  Abnormal termination'
      END IF
      IF ( .NOT. KSNGCHK ) THEN
           CALL FERR ( INT2(3199), &
     &         'Mandatory keyword SINGULARITY_CHECK missed in '// &
     &         '$SETUP section of the control file', INT2(0), INT2(0) )
           STOP 'BATCH(gsetup)  Abnormal termination'
      END IF
!
!      IF ( AOC_USE .EQ. AOC__USE  .OR.  AOC_USE .EQ. AOC__REQ ) THEN
!!
!! -------- Check incompatibility of AOC and FAST_MODE
!!
!           IF ( FAST_MODE .NE. F__NONE ) THEN
!                IP = IFIND_PL ( FM_VAR, FM_VAL, FAST_MODE )
!                IF ( IP .LE. 0 ) IP = 1
!                CALL CLRCH ( STR )
!                CALL ERR_LOG ( 8706, IUER, 'GSETUP', 'Conflicting '// &
!     &              'options in SETUP section of the control file: '// &
!     &              'APRIORI_OBS_CORR is incompatible with FAST_MODE '// &
!     &               STR(1:I_LEN(STR))//'. Please use FAST_MODE NONE' )
!                RETURN 
!           END IF
!      END IF
!
! --- Make sure the control file has specified one of the following
! --- three solution categories:  test (very temporary: e.g., output cgm is
! --- written to a directory usually purged overnight), pre-SOLARCH standard
! --- (more permanent but not catalogued; e.g., output cgm is written to
! --- a stable directory outside the directories reserved for the solution
! --- archiving system) or SOLARCH
! --- (permanent and catalogued; all solution items are catalogued and
! --- placed in the SOLARCH "primary" storage area).  The biggest problem
! --- is that the user will accidentally specify that the solution is both
! --- test and SOLARCH, because he failed to set up his control file properly.
!
      IF ( ( ID(1)(1:4) .EQ. 'TEST'  .OR. ID(1)(1:4) .EQ. 'TEST' ) .AND. &
     &     SOLARCH_SOL ) THEN
           CALL FERR ( INT2(3094), 'BATCH(gsetup) TEST SOLUTIONS CANNOT BE '// &
     &         'CATALOGUED: PLEASE REMOVE USER_TAG & SOL_TAG OR TEST '// &
     &         'FROM ID LINE ', INT2(0), INT2(0) )
      END IF
!
! --- Interpret the CGM line, now that we know what sort of solution this is:
!
! --- First find out which input and output cgms have been requested.
!
      CGML_1 = TRIMLEN(CGMNMR1)
      CGML_2 = TRIMLEN(CGMNMR2)
      CGML_3 = TRIMLEN(CGMNMR3)
      IF ( CGML_1 .EQ. 2  .AND.  CGML_2 .GE. 1 .AND.  CGML_2 .LE. 8 ) THEN
           INCGM_TYPE = 'TAGS'
           INCGM_USER = CGMNMR1(1:CGML_1)
           INCGM_SOL  = CGMNMR2(1:CGML_2)
!@           CALL RESOLVE_CGM ( INCGM_USER, INCGM_SOL, CGMNMR, EMESSAGE, JERR )
!@           IF ( JERR.NE.0 ) THEN
!@!
!@! ------------- Set to background mode so that globl will automatically abort.
!@!
!@                CALL SBIT ( PRE_IP(2), INT2(6), INT2(0) )
!@                CALL FERR ( INT2(3098), EMESSAGE, INT2(0), INT2(0) )
!@           ENDIF
         ELSE IF ( ( CGMNMR1(1:4) .EQ. 'NONE'  .OR.  CGMNMR1(1:4) .EQ. &
     &             'none' ).AND.  CGML_1 .EQ. 4  ) THEN
           INCGM_TYPE = 'NONE'
           CGMNMR     = 'NONE'
           INCGM_USER = '  '
           INCGM_SOL  = '        '
         ELSE IF ( ( CGMNMR1(1:4) .EQ. 'SAVE'  .OR.  CGMNMR1(1:4) .EQ. &
     &             'save' ).AND.  CGML_1 .EQ. 4  ) THEN
           INCGM_TYPE = 'NONE'
           CGMNMR     = 'SAVE'
           INCGM_USER = '  '
           INCGM_SOL  = '        '
         ELSE
           INCGM_TYPE = 'PATH'
           IF ( CGML_1 .GT. 0 ) CGMNMR = CGMNMR1(1:CGML_1)
           INCGM_USER = '  '
           INCGM_SOL  = '        '
      END IF
!
      IF ( INCGM_TYPE .EQ. 'TAGS' ) THEN
           IF ( CGML_3 .EQ. 0) THEN
                OUTCGM = ' '
             ELSE
                IF ( CGML_3 .GT. 0 ) OUTCGM = CGMNMR3(1:CGML_3)
           END IF
        ELSE
           IF ( CGML_2 .EQ. 0 ) THEN
                IF ( CGMNMR .EQ. 'SAVE' .AND. TRIMLEN(CGMNMR) .EQ. 4 ) THEN
                     OUTCGM = 'SAVE'
                   ELSE
                     OUTCGM = 'NONE'
                END IF
             ELSE
                OUTCGM = CGMNMR2(1:CGML_2)
                IF ( OUTCGM(1:4) .EQ. 'none' .AND. TRIMLEN(OUTCGM).EQ.4 ) THEN
                     OUTCGM = 'NONE'
                END IF
                IF ( OUTCGM(1:4) .EQ. 'save' .AND. TRIMLEN(OUTCGM).EQ.4 ) THEN
                     OUTCGM = 'SAVE'
                END IF
           END IF
      END IF
!
! --- Now make sure they are acceptable for the three solution categories,
! --- test, pre-SOLARCH standard (non-test) and SOLARCH.
! ---   (Anything goes for pre-SOLARCH standard and test.)
!
      IF ( SOLARCH_SOL ) THEN
!
! -------- Solution is to be catalogued.  Two things to check:
!
! -------- Make sure the user hasn't specified the input cgm as a full path,
! -------- since this permits the use of an uncatalogued cgm,
! -------- which in turn permits the possibility that,
! -------- if the cgm is ever purged,
! -------- a catalogued solution will depend on a lost cgm.
!
! -------- Users can no longer name the output cgms for catalogued solutions.
! -------- Instead, the output cgms will be given standard names which reflect
! -------- the solution's owner and solution tag (515a etc.).  This process
! -------- will however first write the output cgm to an interim scratch file,
! -------- then copy the scratch file to the final file once the solution is
! -------- successfully completed.  Set the output cgm to this interim scratch
! -------- file.
!
           IF ( INCGM_TYPE .EQ. 'PATH') THEN
                CALL FERR ( INT2(3096), &
     &              'BATCH(GSETUP) a solution which will be '// &
     &              'catalogued may not specify the '//'input cgm as a path', &
     &               INT2(0), INT2(0) )
           END IF
           IF ( SOLTYP.EQ.'F'  .OR.  SOLTYP.EQ.'C' .OR. &
     &          SOLTYP.EQ.'S'  .OR.  SOLTYP.EQ.'G'      ) THEN
!
                OUTCGM = 'CGMC'//RUN_INITS
              ELSE
                OUTCGM = ' '
           END IF
      END IF
!
! --- Reading the next control file record...
!
      CALL CFUNRD ( LENGTH, STRING )
!
! --- Add / to end of arc directory names if forgotten by the user.
!
      DO ICT = 1,3
         IPATH = TRIMLEN(B_ARCDIR(ICT))
         IF ( IPATH .GT. 0 ) THEN
              IF ( B_ARCDIR(ICT)(IPATH:IPATH).NE.'/') THEN
                   IF ( IPATH .LT. NAME_SIZE ) THEN
                        B_ARCDIR(ICT)(IPATH+1:IPATH+1) = '/'
                     ELSE
                        CALL FERR ( INT2(174), "BATCH(GSETUP) Arc file "// &
     &                      "directories must end in / and no room to add one", &
     &                       INT2(0), INT2(0) )
                   ENDIF
               ENDIF
          ENDIF
      ENDDO
!
      IF ( TRIMLEN(CGMNMR) .EQ. 0 ) CGMNMR = 'NONE'
      CALL PUT_PROG_NAME_TO_GLBCM ( USER_PROG, USER_BUFF )
!
      CALL USE_GLBFIL ( 'OWC' )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GSETUP  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PUT_PROG_NAME_TO_GLBCM ( USER_PROG, USER_BUFF )
      INCLUDE 'solve.i'
      INCLUDE 'glbcm.i'
      CHARACTER  USER_PROG*(*), USER_BUFF*(*)
      USER_PROG_NAME = USER_PROG 
      USER_PROG_BUFF = USER_BUFF 
      RETURN
      END  !#!  
