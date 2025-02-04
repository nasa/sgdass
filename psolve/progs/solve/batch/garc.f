      SUBROUTINE GARC ( DONE, ORIENT, DBNAME, VER, SCNOUT, ROTFLG, NRTARC, &
     &                  MODETYP, SUPMET_ARC, USER_PROG, EOPCOK, EOPMOD, &
     &                  SOLTYP, MDORCTL, NO_SUPERFILE, USER_BUFF, DO_CLK_REF, &
     &                  NUM_CLK_REF, LIST_CLK_REF, EOP_SUPR, BASDEP_CLO, &
     &                  WEIGHTS_MODE, EOPSIG, EOPRSIG, FL_NOCAL, FL_NOCONT, &
     &                  FL_NOMAP, DBNAME_MES, EOPMID, EOP_EPOCH_MJD, &
     &                  EOP_EPOCH_SEC, GVH, PARU_FILE_ARC, WEIGHT_ALGORITHM, &
     &                  FL_EOP_CONS, FL_EOPR_CONS, EDIT_FILE, AOC_FILE, &
     &                  ADDW_FILE, DTEC_FILE, ADDW_GLB_SCL, &
     &                  FL_DTEC_SBA_USE, DTEC_ERR_SCL_VAR, EXT_ERR_FILE, VCAT )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GARC PROGRAM SPECIFICATION
!
! 1.1 Process next line of the $ARCS section and get the next arc.
!
! 1.2 REFERENCES:
!
! 2.  GARC INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'belev.i'
      INCLUDE 'gvh.i'
      INCLUDE 'vtd.i'
      INCLUDE 'vcat.i'
      TYPE   ( GVH__STRU  ) ::  GVH
      TYPE   ( VCAT__TYPE ) ::  VCAT
!
! 2.2 INPUT Variables:
!
      CHARACTER   SCNOUT*1, SOLTYP*1, WEIGHTS_MODE*1
      CHARACTER   USER_PROG*(*), DBNAME_MES*(*), PARU_FILE_ARC*(*)
!
! SCNOUT - Screen output flag
! USER_PROG - User-specified program
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*1 ORIENT
      CHARACTER*(*) DBNAME, ROTFLG, MODETYP, EOPCOK, EOPMOD, USER_BUFF, &
     &              EOP_SUPR, BASDEP_CLO, EDIT_FILE*(*), AOC_FILE*(*), &
     &              ADDW_FILE*(*), DTEC_FILE*(*), EXT_ERR_FILE*(*)
      INTEGER*2 SUPMET_ARC, VER, NRTARC, MDORCTL, EOPMID
      LOGICAL*2 DONE, NO_SUPERFILE
!
      LOGICAL*2 DO_CLK_REF          ! Do or don't specify ref clock in arc line.
      INTEGER*4 NUM_CLK_REF         ! number of clock reference sites
      CHARACTER*8 LIST_CLK_REF(MAX_ARC_STA) ! List of 8-character names of
      CHARACTER  SUPMET_TAG*16
      LOGICAL*4  FL_NOCAL  ! Do not use any calibrations
      LOGICAL*4  FL_NOCONT ! Do not use any contributions
      LOGICAL*4  FL_NOMAP  ! Do not use any mappings
      LOGICAL*4  FL_COMP_THEO 
      INTEGER*4  WEIGHT_ALGORITHM
      LOGICAL*4  FL_EOP_CONS, FL_EOPR_CONS  
      LOGICAL*2  EQUAL_EFFFREQ
!
      INTEGER*4  EOP_EPOCH_MJD
      REAL*8     TIME_RAD, EOPSIG(3), EOPRSIG(3), EOP_EPOCH_SEC
      INTEGER*2 NUMDB, LDBNAM(5,15), IDBV(15)
      INTEGER*4 IDBE(15)
      INTEGER*4 MBUF, NBUF, IB, IE, J1, J2, J3, J4, J5, IER
      PARAMETER  ( MBUF = 128 )
      CHARACTER  GVF_DB_DIR*128, GVF_ENV_DIR*128, STR16*16
      CHARACTER  CDBNAM(15)*10, BUF(MBUF)*512
      EQUIVALENCE (CDBNAM,LDBNAM(1,1))
!
! DBNAME - Database name
! DONE - TRUE if there are no more arcs
! EOPCOK - TRUE if earth orientation constraints are allowed for this arc
! NRTARC- Earth orientation epoch
! ORIENT - Earth orientation flag (FIXED or FREE?)
! MODETYP - abbreviation of mode type of observables for the arc
! ROTFLG - Earth orientation components override
! VER - Database version number
! EOPMOD - TRUE for eop mod
! SUPMET_ARC   - suppression method for this session only
! WEIGHTS_MODE - weights flag. "A" and "M" means to make weights
! WEIGHT_ALGORITHM -- algorithm for computing weights
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
      INCLUDE 'batme.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'prfil.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcset
!       CALLED SUBROUTINES: cfread
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*10 DBNAML
      CHARACTER    FNAME*128, CBUF*80, ERRSTR*200
      CHARACTER    STRING*8192, STRING_SAVE*8192, STRING_OLD*8192, &
     &             TOKEN*128, SUPFILE_NAME*160, PDATE*14, STR*32, &
     &             VTD_CONF_FILE*128, TEC_SCAL_STR*32, &
     &             TEC_BIAS_STR*32, DTEC_ERR_SCL_STR*32
      INTEGER*2    LENGTH, IDBNAM(6), I, IDUM, IERR, II
      REAL*8       ELVAL_ARC_VAL, ADDW_GLB_SCL, DTEC_ERR_SCL_VAR
!
      INTEGER*4    IOS, IP, IL, VER_I4, IUER
      INTEGER*2    CFREAD, DECIMALTOINT, TRIMLEN
      LOGICAL*2    CFEOF
      LOGICAL*4    BATCH_MODE, LEX, FL_SOU_USE_DB_IGNORE, FL_DTEC_SBA_USE
      EQUIVALENCE (IDBNAM,DBNAML)
      INTEGER*2    INT2_ARG
      INTEGER*4    INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*2,   EXTERNAL :: KBIT
      INTEGER*4,   EXTERNAL :: I_LEN, ILEN, LINDEX
      INTEGER*8, EXTERNAL :: GET_MEMRSS
!CCCCC
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   910515 Enhanced error messages written to the error file.
!   AEE   911121 Added superfile not found error message.
!   AEE   911203 Allow for comments (start with !) at the end of ARC lines.
!   jwr   971101 Support for reference clocks in arc line.
!   PET   971125 addeed some comments
!   PET   971211 addeed generating the line with database name/version in
!                ba2cm.i
!   PET   970731 Improved error messages. Improved comments. Added support
!                of the keyword SUPPRESS_XYULPE for arc-files.
!                Added a formal parameter EOP_SUPR for suppression of EOP
!                component for arc. String EOP_SUPR has 6 characters: first for
!                X-pole, 2-nd for Y-pole, 3-rd for UT1, 4-th for LOD,
!                5-th for psi nutation offset, 6-th for eps nutation offset.
!                If the n-th element of the string EOP_SUPR is "Y" then that
!                component of EOP-vector will not be estimated in the solution
!                regardless the status flags set in $FLAG section.
!                For example, EOP_SUPR = 'YNNYNY' forbids to estimate
!                X-pole, UT1-rate, eps nutation. Estimation status of other
!                EOP-parameters (Y-pole, UT1, nutation psi) remains untouched.
!   PET   980815 Improved error message when superfile is not found.
!   pet   990104 Added support of the keyword BASDEP_CLO for arc-file. It
!                specifies estimation of baseline-dependent clocks for this
!                arc only. String BASDEP_CLO may have values: "   " -- use
!                global flag of estimation of baseline dependent clocks;
!                "YES" -- estimate all linearly independent baseline dependent
!                clocks; "NO" -- not to estimate; "IN" estimate
!                baseline-dependent clock in according with status recorded
!                in the database.
!   pet   990115 Added call of alternative GTSUP -- a subroutine
!                trans_superfile when SOLVE is working in NO TRAIN mode
!   pet   990407 Improved error messages handling. If the superfile is missed
!                then warning message is printed in the screen and in spool
!                file if and only if G_WARNING is .TRUE. in NO TRIAN mode
!   pet   990429 Added initialization of LSITC when STATIONS NO was specified.
!                The previous version of BATCH set estimation of station
!                positions even if STATIONS NO was specified.
!   pet   1999.11.18  Added support of a keyword TYPE in the arc-line
!   pet   1999.11.26  Correected a bug: added SPLITSTRING after BASDEP_CLO
!                     parsing
!   pet   2000.04.04  Correocted a bug: the previous versiuon didn't allow
!                     to specify stations with blanks in the name in the list
!                     of excluded stations in the arc-line of the control file
!   pet   2000.05.10  Made qualifiers FREE/FIXED optional.
!   pet   2000.11.13  Added support the option IN_EOP_CONSTRAINT of the
!                     arc-line. Then this opotion is ineffect EOP constraints
!                     are set in according to the covaraince matrix in the eop
!                     modfile.
!   pet  2001.05.31   Changed the logic so that it reads obsfil when it processes
!                     the experiment in "make weights" mode
!   pet  2003.10.15   Chaged logic of parsing session option. The old logic 
!                     required them to appear in a certain order. The new logic
!                     allows them to appear in an arbitrary order.
!   pet  2004.03.16   Significantly increased the number of session-line 
!                     options.
!   pet  2005.09.04   Added support of SUPMET_ARC variable
!   pet  2006.02.08   Added support of keywords NOCAL, NOCONT, NOMAP
!   pet  2006.02.09   Added support of keyword VTD_CONF, NO_VTD
!   pet  2007.03.24   Added support of keyword EOP_DAYOFTIME_EPOCH
!   pet  2007.09.24   Added support of keyword EOPR_CONSTRAINT
!   pet  2008.02.25   Added support of keyword ELEVATION
!   pet  2013.06.11   Added support of keyword SNR_MIN
!   pet  2018.09.07   Added support of keyword IONO_ERR_FCT
!   pet  2020.04.27   Added support of the edit file
!   pet  2020.06.24   Added support of STA_ON and STA_INCLUDE keywords in the session line
!   pet  2020.07.14   Added support of additive weight file
!   pet  2022.06.26   Added support of TEC_SCAL, TEC_BIAS variables
!   pet  2022.09.02   Added support of ADDW_GLB_SCL  variable
!   pet  2024.11.30   Added support of ADDW_USE variable
!
! 5.  GARC PROGRAM STRUCTURE
!
!  Get the latest spool info
!
      CALL USE_COMMON ( 'ORC' )
!
! --- Default for no clock reference in arc line and no stations to be excluded
!
      DO_CLK_REF  = .FALSE.
      NUM_CLK_REF = 0
      NUM_STAEXC  = 0
      NUM_STAINC  = 0
      NUM_STAOFF  = 0
      NUM_STAON   = 0
      CALL CLRCH ( MODETYP )
      BASDEP_CLO = '   '
      SUPMET_ARC = SUPMET__UND
      CALL CLRCH ( VTD_CONF_SES ) 
      CALL CLRCH ( EDIT_FILE    ) 
      CALL CLRCH ( AOC_FILE     )
      CALL CLRCH ( ADDW_FILE    )
      CALL CLRCH ( DTEC_FILE    )
      CALL CLRCH ( EXT_ERR_FILE )
      FL_VTD_SES = .FALSE.
      NEL_ARC  = 0 
      TEC_SCAL = 0.0D0
      TEC_BIAS = 0.0D0
      ADDW_GLB_SCL = 1.0D0
      ADDW_USE = ADDW__UNDF
      FL_DTEC_SBA_USE  = .FALSE.
      DTEC_ERR_SCL_VAR = 1.0D0
!
! --- Read the next line of the control file
!
      CALL CLRCH ( STRING )
      LENGTH=CFREAD(STRING)
      STRING_SAVE = STRING
      IP = INDEX ( STRING, '!' ) 
      IF ( IP .GT. 0 ) CALL CLRCH ( STRING(IP:) )
!
! --- If no more arc lines, we're done
!
      DONE = STRING(1:1).EQ.'$' .OR. CFEOF(IDUM)
      IF ( DONE ) THEN
           CALL CFUNRD(LENGTH,STRING )
           RETURN
      ENDIF
!
! --- Get the database name
!
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      DBNAML=TOKEN(1:10)
      IF ( TOKEN(1:1) .EQ. '/' ) THEN
           NO_SUPERFILE = .TRUE.
           USER_BUFF    = 'Y'//TOKEN
           ORIENT       = 'Y'
           PRE_IP(4) = 0
           NRTARC=0
           RETURN
        ELSE
           NO_SUPERFILE = .FALSE.
      ENDIF
!
      IF ( DBNAML(1:1) .EQ. ' ' ) THEN
           DBNAML = ' '
           STRING = ' '
      ENDIF
      FL_NOCAL  = .FALSE.
      FL_NOCONT = .FALSE.
      FL_NOMAP  = .FALSE.
!
      DBNAME = DBNAML
      IF ( INDEX('0123456789',DBNAML(1:1)) .EQ.0 ) DBNAML=DBNAML(2:)
!
! --- Get the version number
!
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      IF ( TOKEN(1:1) .EQ. ' ' ) THEN
           TOKEN  = ' '
           STRING = ' '
      ENDIF
!
      IDBNAM(6) = DECIMALTOINT ( TOKEN, IERR )
      VER = IDBNAM(6)
!
! --- Default flags
!
      ORIENT = 'Y'
      EOPCOK = 'Y'
      EOPMOD = 'Y'
      MDORCTL = 0
      NUM_ATMOFF = 0
      DO_CLK_REF = .FALSE.
      ROTFLG     = ' '
      EOP_SUPR   = '      '
      NRTARC     = 0
      MODETYP    = ' '
      FL_EOP_CONS  = .FALSE.
      FL_EOPR_CONS = .FALSE.
!
! --- Initializtion of batme.i
!
      NUM_ATMOFF  = 0
      NUM_ATMOFF  = 0
      NUM_GRADOFF = 0
      NUM_STAEXC  = 0
      NUM_STAOFF  = 0
      NUM_STAINC  = 0
      NUM_STAON   = 0
      NUM_SOUOFF  = 0
      NUM_SOUEXC  = 0
!
      CALL NOUT ( INT4(MAX_ARC_STA)*8, LIST_ATMOFF  )
      CALL NOUT ( INT4(MAX_ARC_STA)*8, LIST_ATMEXC  )
      CALL NOUT ( INT4(MAX_ARC_STA)*8, LIST_GRADOFF )
      CALL NOUT ( INT4(MAX_ARC_STA)*8, LIST_STAEXC  )
      CALL NOUT ( INT4(MAX_ARC_STA)*8, LIST_STAINC  )
      CALL NOUT ( INT4(MAX_ARC_STA)*8, LIST_STAOFF  )
      CALL NOUT ( INT4(MAX_ARC_STA)*8, LIST_STAON   )
      CALL NOUT ( INT4(MAX_ARC_SRC)*8, LIST_SOUEXC  )
      CALL NOUT ( INT4(MAX_ARC_SRC)*8, LIST_SOUOFF  )
!      
      CLOCK_MD_TYP   = ' '
      CLOCK_INTV_TYP = ' '
      CLOCK_CNS_TYP  = ' '
      GRAD_INTV_TYP  = ' '
      ATM_INTV_TYP   = ' '
      ATM_CNS_TYP    = ' '
      GRAD_CNS_TYP   = ' '
!
      CLOCK_MD_VAL   = 0
!
      CLOCK_INT_VAL  = 0.0D0
      CLOCK_CNS_VAL  = 0.0D0
      ATM_INT_VAL    = 0.0D0
      ATM_CNS_VAL    = 0.0D0
      GRAD_INT_VAL   = 0.0D0
      GRAD_CNS_OFFS  = 0.0D0
      GRAD_CNS_RATE  = 0.0D0
      SESS_REWEI_SCALE = 1.0D0
      SESS_REWEI_QUADR = 0.0D0
!
      SNR_MIN_X = GLO_SNR_MIN_X
      SNR_MIN_S = GLO_SNR_MIN_S
      IONO_ERR_FCT = 0.0D0
!
      CALL CLRCH ( PARU_FILE_ARC )
      FL_SOU_USE_DB_IGNORE = .FALSE.
!
      DO 410 J1=1,32
!
! ------ Get session depedent options
!
         CALL SPLITSTRING ( STRING, TOKEN, STRING )
         IF ( ILEN(TOKEN) .EQ. 0 ) GOTO 810 ! No more options
!
         IF ( TOKEN(1:1) .EQ. '@' ) THEN
!
! =========== @-incluide file
!
! ----------- This option means that further tokens are to be read from the
! ----------- file with the name just followed @
!
              FNAME = TOKEN(2:)
              IUER = -1
              CALL RD_TEXT ( FNAME, MBUF, BUF, NBUF, IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4311, IUER, 'GARC', 'Error in parsing '// &
     &                 'included file in the session line '//FNAME )
                   CALL FERR ( INT2(9010), 'BATCH(garc) Error in '// &
     &                 'processing options for database '//DBNAME, INT2(0), &
     &                  INT2(0) )
                   CALL EXIT ( 1 ) 
              END IF
              STRING_OLD = STRING
              CALL CLRCH ( STRING )
!
! ----------- Combine all non-comments part of lines 
!
              DO 420 J2=1,NBUF
                 IL = I_LEN(BUF(J2))
                 IP = INDEX ( BUF(J2)(1:IL), '!' )
                 IF ( IP .GT. 0 ) IL = IP-1
                 IP = INDEX ( BUF(J2)(1:IL), '\' )
                 IF ( IP .GT. 0 ) IL = IP-1
                 IF ( J2 .EQ. 1 ) THEN
                      STRING = BUF(J2)(1:IL)
                    ELSE 
                      STRING = STRING(1:I_LEN(STRING))//' '//BUF(J2)(1:IL)
                 END IF
 420          CONTINUE 
              STRING = STRING(1:I_LEN(STRING))//' '//STRING_OLD
            ELSE IF ( TOKEN .EQ. 'FREE' ) THEN
!
! =========== FREE
!
! ----------- Obsolete option. Not recommended for use
!
              ORIENT = 'Y'
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
            ELSE IF ( TOKEN .EQ. 'FIXED' ) THEN
!
! =========== FIXED
!
! ----------- Obsolete option. Not recommended
!
              ORIENT = 'N'
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
            ELSE IF ( TOKEN(1:5) .EQ. 'EDIT ' ) THEN
              CALL SPLITSTRING ( STRING, EDIT_FILE, STRING )
              INQUIRE ( FILE=EDIT_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4312, IUER, 'GARC', 'Error in parsing '// &
     &                 'the option EDIT_FILE: file '//TRIM(EDIT_FILE)// &
     &                 ' was not found' )
                   CALL FERR ( INT2(9012), 'BATCH(garc) Error in '// &
     &                 'processing options for database '//DBNAME, INT2(0), &
     &                  INT2(0) )
                   CALL EXIT ( 1 ) 
              END IF
            ELSE IF ( TOKEN(1:5) .EQ. 'AOC ' ) THEN
              CALL SPLITSTRING ( STRING, AOC_FILE, STRING )
              INQUIRE ( FILE=AOC_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4313, IUER, 'GARC', 'Error in parsing '// &
     &                 'the option AOC_FILE: file '//TRIM(AOC_FILE)// &
     &                 ' was not found' )
                   CALL FERR ( INT2(9013), 'BATCH(garc) Error in '// &
     &                 'processing options for database '//DBNAME, INT2(0), &
     &                  INT2(0) )
                   CALL EXIT ( 1 ) 
              END IF
            ELSE IF ( TOKEN(1:5) .EQ. 'ADDW ' ) THEN
              CALL SPLITSTRING ( STRING, ADDW_FILE, STRING )
              INQUIRE ( FILE=ADDW_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4314, IUER, 'GARC', 'Error in parsing '// &
     &                 'the option ADDW_FILE: file '//TRIM(ADDW_FILE)// &
     &                 ' was not found' )
                   CALL FERR ( INT2(9014), 'BATCH(garc) Error in '// &
     &                 'processing options for database '//DBNAME, INT2(0), &
     &                  INT2(0) )
                   CALL EXIT ( 1 ) 
              END IF
            ELSE IF ( TOKEN(1:9) .EQ. 'ADDW_USE ' ) THEN
              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
              IF ( TOKEN(1:4) == 'REPL' ) THEN
                   ADDW_USE = ADDW__REPL
                 ELSE IF ( TOKEN(1:4) == 'QUAD' .OR. TOKEN(1:11) == 'ADD_IN_QUAD' ) THEN
                   ADDW_USE = ADDW__QUAD
                 ELSE IF ( TOKEN(1:3) == 'MIN'  ) THEN
                   ADDW_USE = ADDW__MIN
                 ELSE IF ( TOKEN(1:3) == 'MAX'  ) THEN
                   ADDW_USE = ADDW__MAX
                 ELSE 
                   IUER = -1
                   CALL ERR_LOG ( 4315, IUER, 'GARC', 'Error in parsing '// &
     &                 'the option ADDW_USE: one of REPL, QUAD, MIN, or MAX '// &
     &                 'was expected, but got '//TOKEN )
                   CALL FERR ( INT2(9014), 'BATCH(garc) Error in '// &
     &                 'processing options for database '//DBNAME, INT2(0), &
     &                  INT2(0) )
                   CALL EXIT ( 1 ) 
              END IF
            ELSE IF ( TOKEN(1:8) .EQ. 'EXT_ERR ' ) THEN
              CALL SPLITSTRING ( STRING, EXT_ERR_FILE, STRING )
              INQUIRE ( FILE=EXT_ERR_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4316, IUER, 'GARC', 'Error in parsing '// &
     &                 'the option EXT_ERR_FILE: file '//TRIM(EXT_ERR_FILE)// &
     &                 ' was not found' )
                   CALL FERR ( INT2(9014), 'BATCH(garc) Error in '// &
     &                 'processing options for database '//DBNAME, INT2(0), &
     &                  INT2(0) )
                   CALL EXIT ( 1 ) 
              END IF
            ELSE IF ( TOKEN(1:9) .EQ. 'ADDW_SCL ' ) THEN
              CALL SPLITSTRING ( STRING, STR, STRING )
              READ ( UNIT=STR, FMT=*, IOSTAT=IER ) ADDW_GLB_SCL
              IF ( IER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4317, IUER, 'GARC', 'Error in parsing '// &
     &                 'the option ADDW_SCL: real value was expected, but got '// &
     &                  STR )
                   CALL FERR ( INT2(9015), 'BATCH(garc) Error in '// &
     &                 'processing options for database '//DBNAME, INT2(0), &
     &                  INT2(0) )
                   CALL EXIT ( 1 ) 
              END IF
            ELSE IF ( TOKEN(1:5) .EQ. 'DTEC ' ) THEN
              CALL SPLITSTRING ( STRING, DTEC_FILE, STRING )
              INQUIRE ( FILE=DTEC_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4318, IUER, 'GARC', 'Error in parsing '// &
     &                 'the option DTEC_FILE: file '//TRIM(DTEC_FILE)// &
     &                 ' was not found' )
                   CALL FERR ( INT2(9016), 'BATCH(garc) Error in '// &
     &                 'processing options for database '//DBNAME, INT2(0), &
     &                  INT2(0) )
                   CALL EXIT ( 1 ) 
              END IF
            ELSE IF ( TOKEN(1:9) .EQ. 'TEC_SCAL ' ) THEN
              CALL SPLITSTRING ( STRING, TEC_SCAL_STR, STRING )
              IF ( ILEN(TEC_SCAL_STR) == 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4319, IUER, 'GARC', 'Error in parsing '// &
     &                 'the option TEC_SCAL: value should follow the keyword' )
                   CALL EXIT ( 1 ) 
              END IF
              READ ( UNIT=TEC_SCAL_STR, FMT=*, IOSTAT=IOS ) TEC_SCAL
              IF ( IOS .NE. 0  ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4320, IUER, 'GARC', 'Error in parsing '// &
     &                 'the value of TEC_SCAL: '//TRIM(TEC_SCAL_STR)// &
     &                 ' a real value was expected' )
                   CALL EXIT ( 1 ) 
              END IF
            ELSE IF ( TOKEN(1:9) .EQ. 'TEC_BIAS ' ) THEN
              CALL SPLITSTRING ( STRING, TEC_BIAS_STR, STRING )
              IF ( ILEN(TEC_BIAS_STR) == 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4321, IUER, 'GARC', 'Error in parsing '// &
     &                 'the option TEC_BIAS: value should follow the keyword' )
                   CALL EXIT ( 1 ) 
              END IF
              READ ( UNIT=TEC_BIAS_STR, FMT=*, IOSTAT=IOS ) TEC_BIAS
              IF ( IOS .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4322, IUER, 'GARC', 'Error in parsing '// &
     &                 'the value of TEC_BIAS: '//TRIM(TEC_BIAS_STR)// &
     &                 ' a real value was expected' )
                   CALL EXIT ( 1 ) 
              END IF
           ELSE IF ( TOKEN(1:12) .EQ. 'DTEC_SBA_USE' ) THEN
              FL_DTEC_SBA_USE = .TRUE.
           ELSE IF ( TOKEN(1:12) .EQ. 'DTEC_ERR_SCL' ) THEN
              CALL SPLITSTRING ( STRING, DTEC_ERR_SCL_STR, STRING )
              READ ( UNIT=DTEC_ERR_SCL_STR, FMT=*, IOSTAT=IOS ) DTEC_ERR_SCL_VAR
              IF ( IOS .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4323, IUER, 'GARC', 'Error in parsing '// &
     &                 'the value of DTEC_ERR_SCL: '//TRIM(TEC_SCAL_STR)// &
     &                 ' a real value was expected' )
                   CALL EXIT ( 1 ) 
              END IF
           ELSE IF ( TOKEN .EQ. 'NO_EOP_CONSTRAINT' ) THEN
!
! =========== NO_EOP_CONSTRAINT
!
! ----------- Not to apply contrstraints on EOP
!
              EOPCOK='N'
            ELSE IF ( TOKEN .EQ. 'IN_EOP_CONSTRAINT' ) THEN
!
! =========== IN_EOP_CONSTRAINT
!
! ----------- Apply constraints on EOP
!
              EOPCOK='I'
            ELSE IF ( TOKEN .EQ. 'EOP_CONSTRAINT' ) THEN
!
! =========== EOP_CONSTRAINT
!
              FL_EOP_CONS  = .TRUE.  
!
! ----------- Apply constraints on EOP
!
              EOPCOK='Y'
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( TOKEN .NE. 'SIGMA' ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4324, IUER, 'GARC', 'Error in parsing '// &
     &                 'the option EOP_CONSTRAINT: value SIGMA should '// &
     &                 'follow, but '//TOKEN(1:I_LEN(TOKEN))//' was found' )
                   CALL FERR ( INT2(9021), 'BATCH(garc) Error in '// &
     &                 'processing options for database '//DBNAME, INT2(0), &
     &                  INT2(0) )
                   CALL EXIT ( 1 ) 
              END IF
              DO II = 1,3
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( ILEN(TOKEN) == 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( INT4(II), STR )
                      IUER = -1
                      CALL ERR_LOG ( 4325, IUER, 'GARC', 'Error in parsing '// &
     &                    'the option EOP_CONSTRAINT: no value for the '// &
     &                     STR(1:I_LEN(STR))//'th component of EOP '// &
     &                    'was found' )
                      CALL FERR ( INT2(9022), 'BATCH(garc) Error in '// &
     &                    'processing options for database '//DBNAME, INT2(0), &
     &                     INT2(0) )
                      CALL EXIT ( 1 ) 
                    ELSE
                      IF ( INDEX ( TOKEN, '.' ) == 0 ) THEN
                           TOKEN = TOKEN(1:I_LEN(TOKEN))//'.0' 
                      END IF
                      READ ( UNIT=TOKEN, FMT='(F12.6)', IOSTAT=IER ) EOPSIG(II)
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( INT4(II), STR )
                           IUER = -1
                           CALL ERR_LOG ( 4326, IUER, 'GARC', 'Error in '// &
     &                        'parsing the option EOP_CONSTRAINT: failure '// &
     &                        'in decoding the '//STR(1:I_LEN(STR))// &
     &                        'th component of reciprocal weight for EOP' )
                           CALL FERR ( INT2(9023), 'BATCH(garc) Error in '// &
     &                         'processing options for database '//DBNAME, &
     &                          INT2(0), INT2(0) )
                           CALL EXIT ( 1 ) 
                      END IF
                 END IF
              END DO
!
! ----------- EOPSIG: masec, masec, msec
!
              EOPSIG(3) = EOPSIG(3)*1000.0D0
            ELSE IF ( TOKEN .EQ. 'EOPR_CONSTRAINT' ) THEN
!
! =========== EOPR_CONSTRAINT
!
              FL_EOPR_CONS = .TRUE.
!
! ----------- Apply constraints on EOP rate
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( TOKEN .NE. 'SIGMA' ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4327, IUER, 'GARC', 'Error in parsing '// &
     &                 'the option EOPR_CONSTRAINT: value SIGMA should '// &
     &                 'follow, but '//TOKEN(1:I_LEN(TOKEN))//' was found' )
                   CALL FERR ( INT2(9024), 'BATCH(garc) Error in '// &
     &                 'processing options for database '//DBNAME, INT2(0), &
     &                  INT2(0) )
                   CALL EXIT ( 1 ) 
              END IF
              DO II = 1,3
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( ILEN(TOKEN) == 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( INT4(II), STR )
                      IUER = -1
                      CALL ERR_LOG ( 4328, IUER, 'GARC', 'Error in parsing '// &
     &                    'the option EOPR_CONSTRAINT: no value for the '// &
     &                     STR(1:I_LEN(STR))//'th component of EOP rate '// &
     &                    'was found' )
                      CALL FERR ( INT2(9025), 'BATCH(garc) Error in '// &
     &                    'processing options for database '//DBNAME, INT2(0), &
     &                     INT2(0) )
                      CALL EXIT ( 1 ) 
                    ELSE
                      IF ( INDEX ( TOKEN, '.' ) == 0 ) THEN
                           TOKEN = TOKEN(1:I_LEN(TOKEN))//'.0' 
                      END IF
                      READ ( UNIT=TOKEN, FMT='(F12.6)', IOSTAT=IER ) EOPRSIG(II)
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( INT4(II), STR )
                           CALL ERR_LOG ( 4329, IUER, 'GARC', 'Error in '// &
     &                        'parsing the option EOPR_CONSTRAINT: failure '// &
     &                        'in decoding the '//STR(1:I_LEN(STR))// &
     &                        'th component of reciprocal weight for EOP' )
                           CALL FERR ( INT2(9026), 'BATCH(garc) Error in '// &
     &                         'processing options for database '//DBNAME, &
     &                          INT2(0), INT2(0) )
                           CALL EXIT ( 1 ) 
                      END IF
                 END IF
              END DO
!
! ----------- EOPRSIG: masec/day, masec/day, msec/day
!
              EOPRSIG(3) = EOPRSIG(3)*1000.0D0
            ELSE IF ( TOKEN .EQ. 'NO_EOP_MOD' ) THEN
!
! =========== NO_EOP_MOD
!
              EOPMOD='N'
            ELSE IF ( TOKEN .EQ. 'MOD_ONLY' ) THEN
!
! =========== MOD_ONLY
!
              MDORCTL = 1
            ELSE IF ( TOKEN .EQ. 'CONTROL_ONLY' ) THEN
!
! =========== CONTROL_ONLY
!
              MDORCTL = 2
            ELSE IF ( TOKEN .EQ. 'ATM_OFF' .OR.    &
     &                TOKEN .EQ. 'ATMOSPHERE_OFFSET' ) THEN
!
! =========== ATMOSPHERE_OFF
!
! ----------- Determine for which stations we are to estimate only 
! ----------- atmosphere offset
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              READ ( UNIT=TOKEN(1:8), FMT=*, IOSTAT=IOS ) NUM_ATMOFF
              IF ( IOS .EQ. 0  .AND.  NUM_ATMOFF .GT. 0 ) THEN
                   DO II = 1,NUM_ATMOFF
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
!@U                      CALL UNDSCR ( TOKEN )
                      LIST_ATMOFF(II) = TOKEN(1:8)
                      IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                           CALL INCH ( INT4(II), STR )
                           CALL FERR ( INT2(9051), 'BATCH(garc) Error in '// &
     &                         'parsing a session-line for database '// &
     &                          DBNAME//'. The '//STR(1:I_LEN(STR))// &
     &                         ' object in the list ATMOSPHERE_OFF '// &
     &                         'is missing', INT2(0), INT2(0) )
                      END IF
                   ENDDO
                 ELSE 
                   NUM_ATMOFF = 1
!@U                   CALL UNDSCR ( TOKEN(1:8) )
                   LIST_ATMOFF(NUM_ATMOFF) = TOKEN(1:8)
              ENDIF
            ELSE IF ( TOKEN .EQ. 'ATMOSPHERE_EXCLUDE' ) THEN
!
! =========== ATMOSPHERE_OFFSET
!
! ----------- Determine for whcih stations we are to turn off estimation of 
! ----------- atmosphere path delay in zenith direction
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              READ ( TOKEN, *, IOSTAT=IOS ) NUM_ATMEXC
              IF ( IOS .EQ. 0  .AND.  NUM_ATMEXC .GT. 0 ) THEN
                   DO II = 1,NUM_ATMEXC
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
!@U                      CALL UNDSCR ( TOKEN )
                      LIST_ATMEXC(II) = TOKEN(1:8)
                      IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                           CALL INCH ( INT4(II), STR )
                           CALL FERR ( INT2(9052), 'BATCH(garc) Error in '// &
     &                         'parsing a session-line for database '// &
     &                          DBNAME//'. The '//STR(1:I_LEN(STR))// &
     &                         ' object in the list ATMOSPHERE_OFFSET '// &
     &                         'is missing', INT2(0), INT2(0) )
                      END IF
                   ENDDO
                 ELSE 
                   NUM_ATMEXC = 1
!@U                   CALL UNDSCR ( TOKEN(1:8) )
                   LIST_ATMEXC(NUM_ATMEXC) = TOKEN(1:8)
              ENDIF
            ELSE IF ( TOKEN .EQ. 'GRAD_OFF' .OR. &
     &                TOKEN .EQ. 'GRADIENTS_OFF' ) THEN
!
! =========== GRADIENTS_OFF
!
! ----------- Determine whether we are to turn off atmospheres for a station
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              READ ( TOKEN, *, IOSTAT=IOS ) NUM_GRADOFF
              IF ( IOS .EQ. 0  .AND.  NUM_GRADOFF .GT. 0 ) THEN
                   DO II = 1,NUM_GRADOFF
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
!@U                      CALL UNDSCR ( TOKEN )
                      LIST_GRADOFF(II) = TOKEN(1:8)
                      IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                           CALL INCH ( INT4(II), STR )
                           CALL FERR ( INT2(9053), 'BATCH(garc) Error in '// &
     &                         'parsing a session-line for database '// &
     &                          DBNAME//'. The '//STR(1:I_LEN(STR))// &
     &                         ' object in the list GRADIENTS_OFF '// &
     &                         'is missing', INT2(0), INT2(0) )
                      END IF
                   ENDDO
                 ELSE 
                   NUM_GRADOFF = 1
!@U                   CALL UNDSCR ( TOKEN(1:8) )
                   LIST_GRADOFF(NUM_GRADOFF) = TOKEN(1:8)
              ENDIF
            ELSE IF ( TOKEN .EQ. 'CLOCK_REF_SITES' ) THEN
!
! =========== CLOCK_REF_SITES
!
! ----------- Check for clock reference sites in arc line.
!
              DO_CLK_REF = .TRUE.
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              READ ( TOKEN, *, IOSTAT=IOS ) NUM_CLK_REF
              IF ( IOS .EQ. 0  .AND.  NUM_CLK_REF .GT. 0 ) THEN
                   DO II = 1,NUM_CLK_REF
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
                      LIST_CLK_REF(II) = TOKEN(1:8)
                      IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                           CALL INCH ( INT4(II), STR )
                           CALL FERR ( INT2(9054), 'BATCH(garc) Error in '// &
     &                         'parsing a session-line for database '// &
     &                          DBNAME//'. The '//STR(1:I_LEN(STR))// &
     &                         ' object in the list CLOCK_REF_SITES '// &
     &                         'is missing', INT2(0), INT2(0) )
                      END IF
                   ENDDO
                 ELSE 
                   NUM_CLK_REF = 1
                   LIST_CLK_REF(1) = TOKEN(1:8)
              END IF
            ELSE IF ( TOKEN .EQ. 'STA_EXCLUDE' ) THEN
!
! =========== STA_EXCLUDE
!
! ----------- Check for station exclusion in arc line
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              READ ( TOKEN, *, IOSTAT=IOS ) NUM_STAEXC
              IF ( IOS .EQ. 0  .AND.  NUM_STAEXC .GT. 0 ) THEN
                   DO II=1,NUM_STAEXC
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
!@U                      CALL UNDSCR      ( TOKEN(1:8) )
                      LIST_STAEXC(II)  = TOKEN(1:8)
                      IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                           CALL INCH ( INT4(II), STR )
                           CALL FERR ( INT2(9055), 'BATCH(garc) Error in '// &
     &                         'parsing a session-line for database '// &
     &                          DBNAME//'. The '//STR(1:I_LEN(STR))// &
     &                         ' object in the list STA_EXCLUDE '// &
     &                         'is missing', INT2(0), INT2(0) )
                      END IF
                   ENDDO
                 ELSE 
                   NUM_STAEXC = 1
!@U                   CALL UNDSCR    ( TOKEN(1:8) )
                   LIST_STAEXC(1) = TOKEN(1:8)
              END IF
            ELSE IF ( TOKEN .EQ. 'STA_INCLUDE' ) THEN
!
! =========== STA_INCLUDE
!
! ----------- Check for station exclusion in arc line
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              READ ( TOKEN, *, IOSTAT=IOS ) NUM_STAINC
              IF ( IOS .EQ. 0  .AND.  NUM_STAINC .GT. 0 ) THEN
                   DO II=1,NUM_STAINC
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
                      LIST_STAINC(II)  = TOKEN(1:8)
                      IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                           CALL INCH ( INT4(II), STR )
                           CALL FERR ( INT2(9056), 'BATCH(garc) Error in '// &
     &                         'parsing a session-line for database '// &
     &                          DBNAME//'. The '//STR(1:I_LEN(STR))// &
     &                         ' object in the list STA_INCLUDE '// &
     &                         'is missing', INT2(0), INT2(0) )
                      END IF
                   ENDDO
                 ELSE 
                   NUM_STAINC = 1
                   LIST_STAINC(1) = TOKEN(1:8)
              END IF
            ELSE IF ( TOKEN .EQ. 'STA_POS_OFF'  .OR.  TOKEN .EQ. 'STA_OFF' ) THEN
!
! =========== STA_OFF
!
! ----------- Check for station exclusion in the sesion line
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              READ ( TOKEN, *, IOSTAT=IOS ) NUM_STAOFF
              IF ( IOS .EQ. 0  .AND.  NUM_STAOFF .GT. 0 ) THEN
                   DO II=1,NUM_STAOFF
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
                      LIST_STAOFF(II) =  TOKEN(1:8)
                      IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                           CALL INCH ( INT4(II), STR )
                           CALL FERR ( INT2(9057), 'BATCH(garc) Error in '// &
     &                         'parsing a session-line for database '// &
     &                          DBNAME//'. The '//STR(1:I_LEN(STR))// &
     &                         ' object in the list STA_OFF '// &
     &                         'is missing', INT2(0), INT2(0) )
                      END IF
                   ENDDO
                 ELSE 
                   NUM_STAOFF = 1
                   LIST_STAOFF(1) = TOKEN(1:8)
              END IF
            ELSE IF ( TOKEN .EQ. 'STA_POS_ON'  .OR.  TOKEN .EQ. 'STA_ON' ) THEN
!
! =========== STA_ON
!
! ----------- Check for station exclusion in the sesion line
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              READ ( TOKEN, *, IOSTAT=IOS ) NUM_STAON
              IF ( IOS .EQ. 0  .AND.  NUM_STAON .GT. 0 ) THEN
                   DO II=1,NUM_STAON
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
                      LIST_STAON(II) =  TOKEN(1:8)
                      IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                           CALL INCH ( INT4(II), STR )
                           CALL FERR ( INT2(9058), 'BATCH(garc) Error in '// &
     &                         'parsing a session-line for database '// &
     &                          DBNAME//'. The '//STR(1:I_LEN(STR))// &
     &                         ' object in the list STA_ON '// &
     &                         'is missing', INT2(0), INT2(0) )
                      END IF
                   ENDDO
                 ELSE 
                   NUM_STAON = 1
                   LIST_STAON(1) = TOKEN(1:8)
              END IF
            ELSE IF ( TOKEN .EQ. 'SOU_EXCLUDE' ) THEN
!
! =========== SOU_EXCLUDE
!
! ----------- Check for source exclusion from data in the session line
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              READ ( TOKEN, *, IOSTAT=IOS ) NUM_SOUEXC
              IF ( IOS .EQ. 0  .AND. NUM_SOUEXC .GT. 0 ) THEN
                   DO II=1,NUM_SOUEXC
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
                      LIST_SOUEXC(II) = TOKEN(1:8)
                      IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                           CALL INCH ( INT4(II), STR )
                           CALL FERR ( INT2(9059), 'BATCH(garc) Error in '// &
     &                         'parsing a session-line for database '// &
     &                          DBNAME//'. The '//STR(1:I_LEN(STR))// &
     &                         ' object in the list SOU_EXCLUDE '// &
     &                         'is missing', INT2(0), INT2(0) )
                      END IF
                   ENDDO
                 ELSE 
                   NUM_SOUEXC = 1
                   LIST_SOUEXC(1) = TOKEN(1:8)
              END IF
            ELSE IF ( TOKEN .EQ. 'SOU_OFF' ) THEN
!
! =========== SOU_OFF
!
! ----------- Check for source exclusion from estimation in the session line
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              READ ( TOKEN, *, IOSTAT=IOS ) NUM_SOUOFF
              IF ( IOS .EQ. 0  .AND.  NUM_SOUOFF .GT. 0 ) THEN
                   DO II=1,NUM_SOUOFF
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
                      LIST_SOUOFF(II) = TOKEN(1:8)
                      IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                           CALL INCH ( INT4(II), STR )
                           CALL FERR ( INT2(9060), 'BATCH(garc) Error in '// &
     &                         'parsing a session-line for database '// &
     &                          DBNAME//'. The '//STR(1:I_LEN(STR))// &
     &                         ' object in the list SOU_OFF '// &
     &                         'is missing', INT2(0), INT2(0) )
                      END IF
                   ENDDO
                 ELSE 
                   NUM_SOUOFF = 1
                   LIST_SOUOFF(1) = TOKEN(1:8)
              END IF
            ELSE IF ( TOKEN .EQ. 'BASDEP_CLO' ) THEN
!
! =========== BASDEL_CLO
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              CALL TRAN ( 11, TOKEN(1:3), TOKEN(1:3) )
              IF ( TOKEN(1:1) .EQ. ' '  ) THEN
                   CALL FERR ( INT2(9061), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  'Value of BASDEP_CLO qualifier is not supplied '// &
     &                  'for arc '//DBNAME, INT2(0), INT2(0) )
                   STOP 'BATCH (GARC) Abnormal termination'
              END IF
!
! ----------- Check correctness of the value
!
              IF ( TOKEN(1:3) .EQ. 'YES' .OR. &
     &             TOKEN(1:3) .EQ. 'NO ' .OR. &
     &             TOKEN(1:3) .EQ. 'IN '      ) THEN
!
! ---------------- OK.
!
                   CONTINUE
                 ELSE
                   CALL FERR ( INT2(9062), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  ' Value of BASDEP_CLO qualifier: '// &
     &                  TOKEN(1:I_LEN(TOKEN))//' is not supported '// &
     &                 'value. One of  YES  NO  IN  was expected ', INT2(0), &
     &                 INT2(0) )
                   STOP 'BATCH (GARC) Abnormal termination'
              END IF
!
              BASDEP_CLO = TOKEN(1:3)
            ELSE IF ( TOKEN .EQ. 'TYPE' ) THEN
!
! =========== TYPE
!
! ----------- Get Mode type of observables
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( TOKEN(1:1) .EQ. ' '  ) THEN
                   CALL FERR ( INT2(9063), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  ' No value of TYPE has been supplied', &
     &                  INT2(0), INT2(0) )
                   STOP 'BATCH (GARC) Abnormal termination'
              END IF
              MODETYP = TOKEN
            ELSE IF ( TOKEN .EQ. 'SUPPRESS_XYULPE' ) THEN
!
! =========== SUPPRESS_XYULPE
!
! ----------- Get override on one or more eop components
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
!
              CALL TRAN ( 11, TOKEN(1:6), TOKEN(1:6) )
              IF ( TOKEN(1:1) .EQ. ' '  ) THEN
                   CALL FERR ( INT2(9064), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  'Value of SUPPRESS_XYPLPE qualifier is not '// &
     &                  'supplied', INT2(0), INT2(0) )
                   STOP 'BATCH (GARC) Abnormal termination'
              END IF
!
              DO I=1,6
                 IF ( TOKEN(I:I) .NE. 'Y'  .AND. TOKEN(I:I) .NE. 'N' ) THEN
                      CALL FERR ( INT2(9065), 'BATCH(garc) Error in '// &
     &                    'parsing  a session-line: for database '//DBNAME// &
     &                    ' -- Illegal value of '// &
     &                    'SUPPRESS_XYPLPE qualifier was detected: '// &
     &                     TOKEN(1:6)//' -- only letters N or Y are '// &
     &                    'allowed there', INT2(0), INT2(0) )
                      STOP 'BATCH (GARC) Abnormal termination'
                 END IF
              END DO
!
              EOP_SUPR = TOKEN(1:6)
            ELSE IF ( TOKEN .EQ. 'CLOCK_FLAGS' ) THEN
!
! =========== CLOCK_FLAGS
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                   CALL FERR ( INT2(9066), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  'The first value of the keyword CLOCK_FLAGS '// &
     &                  'is missing ', INT2(0), INT2(0) )
              END IF
              IF ( TOKEN .EQ. 'MAX_DEGREE' ) THEN
                   CONTINUE 
                 ELSE
                   CALL FERR ( INT2(9067), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  'The first value of the keyword CLOCK_FLAGS has '// &
     &                  'a wrong value: '//TOKEN(1:I_LEN(TOKEN))// &
     &                  ' MAX_DEGREE was expected', INT2(0), INT2(0) )
              END IF
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                   CALL FERR ( INT2(9068), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  'The third value of the keyword CLOCK_FLAGS '// &
     &                  'is missing ', INT2(0), INT2(0) )
              END IF
              CLOCK_MD_TYP = TOKEN(1:1)
              IF ( CLOCK_MD_TYP .EQ. 'I' .OR. &
     &             CLOCK_MD_TYP .EQ. 'A' .OR. &
     &             CLOCK_MD_TYP .EQ. 'M'      ) THEN
                   CONTINUE 
                 ELSE
                   CALL FERR ( INT2(9069), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  'the third value of the keyword CLOCK_FLAGS: '// &
     &                   TOKEN(1:I_LEN(TOKEN))//' has a wrong value. '// &
     &                  'I or A or M were expected', INT2(0), INT2(0) )
              END IF
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                   CALL FERR ( INT2(9070), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  'The third value of the keyword CLOCK_FLAGS '// &
     &                  'is missing ', INT2(0), INT2(0) )
              END IF
!
              READ ( UNIT=TOKEN, FMT='(I6)', IOSTAT=IOS ) CLOCK_MD_VAL
              IF ( IOS .NE. 0 ) THEN
                   CALL FERR ( INT2(9071), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  ' -- wrong format of the third value of the '// &
     &                  'CLOCK_FLAGS keyword: '//TOKEN(1:I_LEN(TOKEN))// &
     &                  ' An integer value 1 or 2 was expected', &
     &                  INT2(0), INT2(0) )
              END IF
!
              IF ( CLOCK_MD_VAL .NE. 1 .AND. CLOCK_MD_VAL .NE. 2 ) THEN
                   CALL FERR ( INT2(9072), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  ' -- wrong value of the third value of the '// &
     &                  'CLOCK_FLAGS keyword: '//TOKEN(1:I_LEN(TOKEN))// &
     &                  ' An integer value 1 or 2 was expected', &
     &                  INT2(0), INT2(0) )
              END IF
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                   CALL FERR ( INT2(9060), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  'The fourth value is missing ', INT2(0), INT2(0) )
              END IF
              IF ( TOKEN .EQ. 'INTERVAL'  .OR.  TOKEN .EQ. 'INTERVALS' ) THEN
                   CONTINUE 
                 ELSE
                   CALL FERR ( INT2(9062), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  ' -- wrong value of the fourth value of the '// &
     &                  'CLOCK_FLAGS keyword: '//TOKEN(1:I_LEN(TOKEN))// &
     &                  ' qualifier INTERVAL was expected', INT2(0), INT2(0) )
              END IF
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                   CALL FERR ( INT2(9064), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  'The fifth value of the keyword CLOCK_FLAGS '// &
     &                  'is missing ', INT2(0), INT2(0) )
              END IF
              IF ( TOKEN == 'WEAKEST' ) TOKEN = 'MOST'
              CLOCK_INTV_TYP = TOKEN(1:1)
              IF ( CLOCK_INTV_TYP .EQ. 'A' .OR. &
     &             CLOCK_INTV_TYP .EQ. 'F' .OR. &
     &             CLOCK_INTV_TYP .EQ. 'I' .OR. &
     &             CLOCK_INTV_TYP .EQ. 'N' .OR. &
     &             CLOCK_INTV_TYP .EQ. 'M'      ) THEN
                   CONTINUE 
                 ELSE
                   CALL FERR ( INT2(9066), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  ' -- wrong value of the fifth value of the '// &
     &                  'CLOCK_FLAGS keyword: '//TOKEN(1:I_LEN(TOKEN))// &
     &                  ' AUTO, FORCE, IN, NO or MOST was expected', &
     &                  INT2(0), INT2(0) )
              END IF
!
              IF ( CLOCK_INTV_TYP .EQ. 'A' .OR. &
     &             CLOCK_INTV_TYP .EQ. 'F' .OR. &
     &             CLOCK_INTV_TYP .EQ. 'M'      ) THEN
!
                   CALL SPLITSTRING ( STRING, TOKEN, STRING )
                   IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                        CALL FERR ( INT2(9068), 'BATCH(garc) Error in '// &
     &                      'parsing a session-line for database '//DBNAME// &
     &                      '. The sixth value of the keyword CLOCK_FLAGS '// &
     &                      'is missing ', INT2(0), INT2(0) )
                   END IF
                   IF ( INDEX ( TOKEN, '.' ) .EQ. 0 ) THEN
                        TOKEN  = TOKEN(1:I_LEN(TOKEN))//'.0' 
                   END IF
!
                   READ ( UNIT=TOKEN, FMT='(F12.6)', IOSTAT=IOS ) CLOCK_INT_VAL
                   IF ( IOS .NE. 0 ) THEN
                        CALL FERR ( INT2(9070), 'BATCH(garc) Error in '// &
     &                      'parsing a session-line for database '//DBNAME// &
     &                      '. -- wrong value of the sixth value of the '// &
     &                      'CLOCK_FLAGS keyword: '//TOKEN(1:I_LEN(TOKEN))// &
     &                      ' -- A float number was expected', &
     &                  INT2(0), INT2(0) )
                   END IF
              END IF
            ELSE IF ( TOKEN .EQ. 'CLOCK_CONSTRAINTS' ) THEN
!
! =========== CLOCK_CONSTRAINTS
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                   CALL FERR ( INT2(9072), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  'The first value of the keyword CLOCK_CONSTRAINTS '// &
     &                  'is missing ', INT2(0), INT2(0) )
              END IF
              CLOCK_CNS_TYP = TOKEN(1:1)
              IF ( CLOCK_CNS_TYP .EQ. 'I' .OR. &
     &             CLOCK_CNS_TYP .EQ. 'Y' .OR. &
     &             CLOCK_CNS_TYP .EQ. 'A' .OR. &
     &             CLOCK_CNS_TYP .EQ. 'N' .OR. &
     &             CLOCK_CNS_TYP .EQ. 'M'      ) THEN
                   CONTINUE 
                 ELSE
                   CALL FERR ( INT2(9074), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  ' the qualifier '//TOKEN(1:I_LEN(TOKEN))// &
     &                  ' of the CLOCKS_CONSTRAINTS keyword has '// &
     &                  'a wrong value. I or N or M or A or Y were expected', &
     &                  INT2(0), INT2(0) )
              END IF
!
              IF ( CLOCK_CNS_TYP .NE. 'N'  .AND.  CLOCK_CNS_TYP .NE. 'I' ) THEN
                   CALL SPLITSTRING ( STRING, TOKEN, STRING )
                   IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                        CALL FERR ( INT2(9076), 'BATCH(garc) Error in '// &
     &                      'parsing a session-line for database '//DBNAME// &
     &                      '. The second value of the keyword '// &
     &                      'CLOCK_CONSTRAINTS is missing ', INT2(0), INT2(0) )
                   END IF
                   IF ( INDEX ( TOKEN, '.' ) .EQ. 0 ) THEN
                        TOKEN  = TOKEN(1:I_LEN(TOKEN))//'.0' 
                   END IF
!
                   READ ( UNIT=TOKEN, FMT='(F12.6)', IOSTAT=IOS ) CLOCK_CNS_VAL
                   IF ( IOS .NE. 0 ) THEN
                        CALL FERR ( INT2(9078), 'BATCH(garc) Error in '// &
     &                      'parsing a session-line for database '//DBNAME// &
     &                      '. -- the second value of the '// &
     &                      'CLOCKS_CONSTRIANTS keyword: '// &
     &                      TOKEN(1:I_LEN(TOKEN))//' has a wrong format. '// &
     &                      'A float number was expected', INT2(0), INT2(0) )
                   END IF 
              END IF 
            ELSE IF ( TOKEN .EQ. 'ATMOSPHERE_FLAGS' ) THEN
!
! =========== ATMOSPHERE_FLAGS
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                   CALL FERR ( INT2(9080), 'BATCH(garc) Error in '// &
     &                 'parsing a session-line for database '//DBNAME// &
     &                 '. The first value of the keyword '// &
     &                 'ATMOSPHERE_FLAGS is missing ', INT2(0), INT2(0) )
              END IF
              ATM_INTV_TYP = TOKEN(1:1)
              IF ( TOKEN == 'WEAKEST' ) ATM_INTV_TYP = 'M'
              IF ( ATM_INTV_TYP .EQ. 'A' .OR. &
     &             ATM_INTV_TYP .EQ. 'I' .OR. &
     &             ATM_INTV_TYP .EQ. 'M' .OR. &
     &             ATM_INTV_TYP .EQ. 'N' .OR. &
     &             ATM_INTV_TYP .EQ. 'Y'      ) THEN
                   CONTINUE 
                 ELSE
                   CALL FERR ( INT2(9082), 'BATCH(garc) Error in parsing '// &
     &                  ' a session-line for database '//DBNAME//'. '// &
     &                  ' the first qualifier of the ATMOSPHERE_FLAGS '// &
     &                  'keyword: '//TOKEN(1:I_LEN(TOKEN))//' has a wrong '// &
     &                  'value. A or I or or M or N or Y were expected', &
     &                  INT2(0), INT2(0) )
              END IF
!
              IF ( ATM_INTV_TYP .EQ. 'A'  .OR. &
     &             ATM_INTV_TYP .EQ. 'M'  .OR. &
     &             ATM_INTV_TYP .EQ. 'Y'       ) THEN
!
                   CALL SPLITSTRING ( STRING, TOKEN, STRING )
                   IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                        CALL FERR ( INT2(9084), 'BATCH(garc) Error in '// &
     &                      'parsing a session-line for database '//DBNAME// &
     &                      '. The second value of the keyword '// &
     &                      'ATMOSPHERE_FLAGS is missing ', INT2(0), INT2(0) )
                   END IF
!
                   IF ( TOKEN .EQ. 'INTERVAL' ) THEN
                        CALL SPLITSTRING ( STRING, TOKEN, STRING )
                      ELSE 
                        CALL FERR ( INT2(9086), 'BATCH(garc) Error in '// &
     &                      'parsing a session-line for database '//DBNAME// &
     &                      '. The second value of the keyword '// &
     &                      'ATMOSPHERE_FLAGS is wrong: '// &
     &                       TOKEN(1:I_LEN(TOKEN))//' -- INTERVAL was '// &
     &                      'expected', INT2(0), INT2(0) )
                   END IF
!
                   IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                        CALL FERR ( INT2(9088), 'BATCH(garc) Error in '// &
     &                      'parsing a session-line for database '//DBNAME// &
     &                      '. The third value of the keyword '// &
     &                      'ATMOSPHERE_FLAGS is missing ', INT2(0), INT2(0) )
                   END IF
!
                   IF ( INDEX ( TOKEN, '.' ) .EQ. 0 ) THEN
                        TOKEN  = TOKEN(1:I_LEN(TOKEN))//'.0' 
                   END IF
!
                   READ ( UNIT=TOKEN, FMT='(F12.6)', IOSTAT=IOS ) ATM_INT_VAL
                   IF ( IOS .NE. 0 ) THEN
                        CALL FERR ( INT2(9090), 'BATCH(garc) Error in '// &
     &                      'parsing a session-line for database '// &
     &                       DBNAME//'. '//' -- the second qualifier of '// &
     &                      'the ATMOSPHERE_FLAGS keyword: '// &
     &                       TOKEN(1:I_LEN(TOKEN))//' has a wrong value. '// &
     &                      'A float number was expected', INT2(0), INT2(0) )
                   END IF
              END IF
            ELSE IF ( TOKEN .EQ. 'ATMOSPHERE_CONSTRAINTS' ) THEN
!
! =========== ATMOSPHERE_CONSTRAINTS
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                   CALL FERR ( INT2(9092), 'BATCH(garc) Error in '// &
     &                 'parsing a session-line for database '//DBNAME// &
     &                 '. The first value of the keyword '// &
     &                 'ATMOSPHERE_CONSTRAINTS is missing', INT2(0), INT2(0) )
              END IF
!
              ATM_CNS_TYP = TOKEN(1:1)
              IF ( ATM_CNS_TYP .EQ. 'A' .OR. &
     &             ATM_CNS_TYP .EQ. 'I' .OR. &
     &             ATM_CNS_TYP .EQ. 'M' .OR. &
     &             ATM_CNS_TYP .EQ. 'N' .OR. &
     &             ATM_CNS_TYP .EQ. 'Y'      ) THEN
                   CONTINUE 
                 ELSE
                   CALL FERR ( INT2(9094), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  ' the first values of the keyword '// &
     &                  'ATMOSPHERE_CONSTRAINTS has a wrong value: '// &
     &                  TOKEN(1:I_LEN(TOKEN))//' -- '// &
     &                  'A or I or M or N or Y were expected', &
     &                  INT2(0), INT2(0) )
              END IF
!
              IF ( ATM_CNS_TYP .NE. 'N'  .AND.  ATM_CNS_TYP .NE. 'I' ) THEN
                   CALL SPLITSTRING ( STRING, TOKEN, STRING )
                   IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                        CALL FERR ( INT2(9096), 'BATCH(garc) Error in '// &
     &                      'parsing a session-line for database '//DBNAME// &
     &                      '. The second value of the keyword '// &
     &                      'ATMOSPHERE_CONSTRAINTS is missing ', &
     &                      INT2(0), INT2(0) )
                   END IF
                   IF ( INDEX ( TOKEN, '.' ) .EQ. 0 ) THEN
                        TOKEN  = TOKEN(1:I_LEN(TOKEN))//'.0' 
                   END IF
!
                   READ ( UNIT=TOKEN, FMT='(F12.6)', IOSTAT=IOS ) ATM_CNS_VAL
                   IF ( IOS .NE. 0 ) THEN
                        CALL FERR ( INT2(9098), 'BATCH(garc) Error in '// &
     &                      'parsing a session-line for database '//DBNAME// &
     &                      '. the second values of the keyword '// &
     &                      'ATMOSPHERE_CONSTRAINTS has a wrong format: '// &
     &                      TOKEN(1:I_LEN(TOKEN))//' -- '// &
     &                      'A float number was expected', INT2(0), INT2(0) )
                   END IF
              END IF
            ELSE IF ( TOKEN .EQ. 'GRADIENT_FLAGS' ) THEN
!
! =========== GRADIENT_FLAGS
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                   CALL FERR ( INT2(9100), 'BATCH(garc) Error in '// &
     &                 'parsing a session-line for database '//DBNAME// &
     &                 '. The first value of the keyword '// &
     &                 'GRADIENT_FLAGS is missing', INT2(0), INT2(0) )
              END IF
!
              GRAD_INTV_TYP = TOKEN(1:1)
              IF ( GRAD_INTV_TYP .EQ. 'A' .OR. &
     &             GRAD_INTV_TYP .EQ. 'N' .OR. &
     &             GRAD_INTV_TYP .EQ. 'Y'      ) THEN
                   CONTINUE 
                 ELSE
                   CALL FERR ( INT2(9102), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  'the first value of the keyword GRADIENT_FLAGS: '// &
     &                  TOKEN(1:I_LEN(TOKEN))//' has a wrong value. '// &
     &                  'A or N or Y were expected', INT2(0), INT2(0) )
              END IF
!
              IF ( GRAD_INTV_TYP .EQ. 'A' .OR. &
     &             GRAD_INTV_TYP .EQ. 'Y'      ) THEN
                   CALL SPLITSTRING ( STRING, TOKEN, STRING )
                   IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                        CALL FERR ( INT2(9104), 'BATCH(garc) Error in '// &
     &                      'parsing a session-line for database '//DBNAME// &
     &                      '. The second value of the keyword '// &
     &                      'GRADIENT_FLAGS is missing ', INT2(0), INT2(0) )
                   END IF
!
                   IF ( TOKEN .EQ. 'INTERVAL' ) THEN
                        CALL SPLITSTRING ( STRING, TOKEN, STRING )
                      ELSE 
                        CALL FERR ( INT2(9106), 'BATCH(garc) Error in '// &
     &                      'parsing a session-line for database '//DBNAME// &
     &                      '. The second value of the keyword '// &
     &                      'GRADIENT_FLAGS is wrong: '// &
     &                       TOKEN(1:I_LEN(TOKEN))//' -- INTERVAL was '// &
     &                      'expected', INT2(0), INT2(0) )
                   END IF
!
                   IF ( INDEX ( TOKEN, '.' ) .EQ. 0 ) THEN
                        TOKEN  = TOKEN(1:I_LEN(TOKEN))//'.0' 
                   END IF
!
                   READ ( UNIT=TOKEN, FMT='(F12.6)', IOSTAT=IOS ) GRAD_INT_VAL
                   IF ( IOS .NE. 0 ) THEN
                        CALL FERR ( INT2(9108), 'BATCH(garc) Error in '// &
     &                      'parsing a session-line for database '// &
     &                       DBNAME//'. '//' -- the third value of the '// &
     &                      'keyword GRADIENT_FLAGS: '// &
     &                       TOKEN(1:I_LEN(TOKEN))//' has a wrong value '// &
     &                      'A float number was expected', &
     &                       INT2(0), INT2(0) )
                   END IF
              END IF
            ELSE IF ( TOKEN .EQ. 'GRADIENT_CONSTRAINTS' ) THEN
!
! =========== GRADIENT_CONSTRAINTS
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                   CALL FERR ( INT2(9110), 'BATCH(garc) Error in '// &
     &                 'parsing a session-line for database '//DBNAME// &
     &                 '. The first value of the keyword '// &
     &                 'GRADIENT_CONSTRATINS is missing', INT2(0), INT2(0) )
              END IF
!
              GRAD_CNS_TYP = TOKEN(1:1)
              IF ( GRAD_CNS_TYP .EQ. 'N' .OR. &
     &             GRAD_CNS_TYP .EQ. 'A' .OR. &
     &             GRAD_CNS_TYP .EQ. 'Y'      ) THEN
                   CONTINUE 
                 ELSE
                   CALL FERR ( INT2(9112), 'BATCH(garc) Error in parsing '// &
     &                  'a session-line for database '//DBNAME//'. '// &
     &                  ' the first value of the keyword '// &
     &                  'GRADIENT_CONSTRAINTS: '//TOKEN(1:I_LEN(TOKEN))// &
     &                  ' has a wrong value. N or Y were expected', &
     &                  INT2(0), INT2(0) )
              END IF
!
              IF ( GRAD_CNS_TYP .EQ. 'A' .OR. &
     &             GRAD_CNS_TYP .EQ. 'M'      ) THEN
!
                   CALL SPLITSTRING ( STRING, TOKEN, STRING )
                   IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                        CALL FERR ( INT2(9114), 'BATCH(garc) Error in '// &
     &                      'parsing a session-line for database '//DBNAME// &
     &                      '. The second value of the keyword '// &
     &                      'GRADIENT_CONSTRATINS is missing', &
     &                      INT2(0), INT2(0) )
                   END IF
!
                   IF ( INDEX ( TOKEN, '.' ) .EQ. 0 ) THEN
                        TOKEN  = TOKEN(1:I_LEN(TOKEN))//'.0' 
                   END IF
!
                   READ ( UNIT=TOKEN, FMT='(F12.6)', IOSTAT=IOS ) GRAD_CNS_OFFS
                   IF ( IOS .NE. 0 ) THEN
                        CALL FERR ( INT2(9116), 'BATCH(garc) Error in '// &
     &                      'parsing a session-line for database '//DBNAME// &
     &                      '. The second value of the keyword '// &
     &                      'GRADIENT_CONSTRAINTS: has a wrong format. '// &
     &                      TOKEN(1:I_LEN(TOKEN))//' A float number was '// &
     &                      'expected', INT2(0), INT2(0) )
                   END IF
!
                   CALL SPLITSTRING ( STRING, TOKEN, STRING )
                   IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                        CALL FERR ( INT2(9118), 'BATCH(garc) Error in '// &
     &                      'parsing a session-line for database '//DBNAME// &
     &                      '. The third value of the keyword '// &
     &                      'GRADIENT_CONSTRATINS is missing', &
     &                      INT2(0), INT2(0) )
                   END IF
                   IF ( INDEX ( TOKEN, '.' ) .EQ. 0 ) THEN
                        TOKEN  = TOKEN(1:I_LEN(TOKEN))//'.0' 
                   END IF
!
                   READ ( UNIT=TOKEN, FMT='(F12.6)', IOSTAT=IOS ) GRAD_CNS_RATE
                   IF ( IOS .NE. 0 ) THEN
                        CALL FERR ( INT2(9120), 'BATCH(garc) Error in '// &
     &                      'parsing a session-line for database '//DBNAME// &
     &                      '. The third value of the keyword '// &
     &                      'GRADIENT_CONSTRAINTS: has a wrong format. '// &
     &                      TOKEN(1:I_LEN(TOKEN))//' A float number was '// &
     &                      'expected', INT2(0), INT2(0) )
                   END IF
              END IF
            ELSE IF ( TOKEN .EQ. 'SUPMET' ) THEN
!
! =========== SUPMET
!
              CALL SPLIT_AND_CASEFOLD ( STRING, SUPMET_TAG, STRING )
              DO 430 J3=SUPMET__FIRST, SUPMET__LAST
!
! -------------- Get ascii description of the suppression method with
! -------------- code J3
!
                 CALL SUPMET_SHOW ( INT2(J3), STR )
!
! -------------- Extract there code itself
!
                 IB = INDEX ( STR, '__' ) + 2
                 IE = INDEX ( STR, ' '  ) - 1
                 IF ( IE.GT.IB ) THEN
!
! ------------------- Search occurrence of the code
!
                      IF ( INDEX ( SUPMET_TAG, STR(IB:IE) ) .GT. 0 ) THEN
                           IF ( SUPMET_ARC .NE. SUPMET__UND ) THEN
                                IUER = -1
                                CALL ERR_LOG ( 4330, IUER, 'GARC', &
     &                            'Ambiguous code of suppression method: '// &
     &                             SUPMET_TAG//' while processing '// &
     &                            'arc line '//STRING_SAVE )
                                STOP 'BATCH(garc)  abnormal termination'
                           END IF
                           SUPMET_ARC = J3
                      END IF
                 END IF
 430          CONTINUE
!
              IF ( SUPMET_ARC .EQ. SUPMET__UND  .AND. &
     &             SUPMET_TAG(1:3) .NE. 'UND'          ) THEN
!
                   IUER = -1
                   CALL ERR_LOG ( 4331, IUER, 'GARC', 'Wrong '// &
     &                 'SUPMET value: '//SUPMET_TAG(1:12) )
                   STOP 'BATCH(garc)  abnormal termination'
              END IF
            ELSE IF ( TOKEN(1:5) .EQ. 'NOCAL'  ) THEN
              FL_NOCAL  = .TRUE.
            ELSE IF ( TOKEN(1:6) .EQ. 'NOCONT' ) THEN
              FL_NOCONT = .TRUE.
            ELSE IF ( TOKEN(1:5) .EQ. 'NOMAP'  ) THEN
              FL_NOMAP  = .TRUE.
            ELSE IF ( TOKEN(1:8) .EQ. 'VTD_CONF'  ) THEN
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( ILEN(TOKEN) .EQ. 0 ) THEN
                   CALL FERR ( INT2(9122), 'BATCH(garc) Error in '// &
     &                 'parsing a session-line for database '//DBNAME// &
     &                 '. The second value of the keyword '// &
     &                 'VTD_CONF is missing', INT2(0), INT2(0) )
              END IF
              VTD_CONF_SES = TOKEN
              FL_VTD_SES = .TRUE.
              INQUIRE ( FILE=VTD_CONF_SES, EXIST=LEX ) 
              IF ( .NOT. LEX ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4332, IUER, 'GARC', 'Cannot find VTD '// &
     &                 'configuratin file '//VTD_CONF_SES(1:I_LEN(VTD_CONF_SES))// &
     &                 ' specified in the session line for database '//DBNAME )
                   STOP 'BATCH(garc)  abnormal termination'
              END IF
            ELSE IF ( TOKEN .EQ. 'EOP_DAYOFTIME_EPOCH' ) THEN
!
! =========== Epoch fo EOP estimation
!
              EOPMID = 5 ! EOP epoch as a day of time
              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
              IER = -1
              CALL HR_TAT ( TOKEN, TIME_RAD, IER )
              IF ( IER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4333, IUER, 'GARC', 'Error in '// &
     &                 'processing the value of the qualifier '// &
     &                 'EOP_DAYOFTIME_EPOCH '//TOKEN(1:I_LEN(TOKEN))// &
     &                 ' during parsing a session-line for database '// &
     &                 DBNAME )
                   CALL EXIT ( 1 )
              END IF
              CALL RS_TAT ( TIME_RAD, EOP_EPOCH_SEC )
              EOP_EPOCH_MJD = 0
            ELSE IF ( TOKEN .EQ. 'EOP_EPOCH' ) THEN
!
! =========== Epoch fo EOP estimation
!
              EOPMID = 7 ! EOP epoch is specified directly
              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
              IER = -1
              CALL DATE_TO_TIME ( TOKEN, EOP_EPOCH_MJD, EOP_EPOCH_SEC, IER )
              IF ( IER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4334, IUER, 'GARC', 'Error in '// &
     &                 'processing the value of the qualifier '// &
     &                 'EOP_DAYOFTIME_EPOCH '//TOKEN(1:I_LEN(TOKEN))// &
     &                 ' during parsing a session-line for database '// &
     &                 DBNAME )
                   CALL EXIT ( 1 )
              END IF
            ELSE IF ( TOKEN .EQ. 'PARU_FILE' ) THEN
!
! =========== File name for PARU
!
              CALL SPLITSTRING ( STRING, PARU_FILE_ARC, STRING )
            ELSE IF ( TOKEN .EQ. 'SOU_USE_DB_IGNORE' ) THEN
!
! =========== Ignore source uage flags sotred in the database
!
              FL_SOU_USE_DB_IGNORE = .TRUE.
            ELSE IF ( TOKEN .EQ. 'ELEVATION' ) THEN
!
! ----------- Process ELEVATION(-cutoff angle) option
!
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              READ ( UNIT=TOKEN, FMT='(F12.6)', IOSTAT=IER ) ELVAL_ARC_VAL
              IF ( IER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4335, IUER, 'GARC', 'Error in '// &
     &                 'processing the value of the qualifier '// &
     &                 'ELEVATIOM '//TOKEN(1:I_LEN(TOKEN))// &
     &                 ' during parsing a session-line for database '// &
     &                 DBNAME )
                   CALL EXIT ( 1 )
              END IF
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( TOKEN == 'ALL' ) THEN
                   NEL_ARC = 1
                   ELVAL_ARC(1) = ELVAL_ARC_VAL
                   ELNM_ARC_CHR(1) = 'ALL     '
                 ELSE 
                   READ ( UNIT=TOKEN, FMT='(I6)', IOSTAT=IER ) NEL_ARC
                   IF ( IER .NE. 0 ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 4336, IUER, 'GARC', 'Error in '// &
     &                      'processing the second value of the qualifier '// &
     &                      'ELEVATIOM '//TOKEN(1:I_LEN(TOKEN))// &
     &                      ' during parsing a session-line for database '// &
     &                      DBNAME//' -- ALL or an integered were expected' )
                        CALL EXIT ( 1 )
                   END IF
!
                   DO 440 J4=1,NEL_ARC
                      CALL SPLITSTRING ( STRING, ELNM_ARC_CHR(J4), STRING )
!@U                      CALL UNDSCR ( ELNM_ARC_CHR(J4) )
                      ELVAL_ARC(J4) = ELVAL_ARC_VAL
 440               CONTINUE 
              END IF
            ELSE IF ( TOKEN .EQ. 'SNR_MIN' ) THEN
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              READ ( UNIT=TOKEN, FMT='(F10.2)', IOSTAT=IER ) SNR_MIN_X
              IF ( IER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4337, IUER, 'GARC', 'Error in '// &
     &                 'parsing SNR_MIN keyword: two real number were '// &
     &                 'expected, but the first number is '//TOKEN )
                   CALL EXIT ( 1 )
              END IF
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( ILEN(TOKEN) == 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4338, IUER, 'GARC', 'Error in '// &
     &                 'parsing SNR_MIN keyword: '// &
     &                 'two real numbers should follow, but only one '// &
     &                 'was spectied' )
                   CALL EXIT ( 1 )
              END IF
              READ ( UNIT=TOKEN, FMT='(F10.2)', IOSTAT=IER ) SNR_MIN_S
              IF ( IER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4339, IUER, 'GARC', 'Error in '// &
     &                 'parsing SNR_MIN keyword: two real number were '// &
     &                 'expected, but the second number is '//TOKEN )
                   CALL EXIT ( 1 )
              END IF
            ELSE IF ( TOKEN .EQ. 'IONO_ERR' ) THEN
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              READ ( UNIT=TOKEN, FMT='(F10.5)', IOSTAT=IER ) IONO_ERR_FCT
              IF ( IER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4340, IUER, 'GARC', 'Error in '// &
     &                 'parsing IONO_ERR keyword: two real number were '// &
     &                 'expected, but the first number is '//TOKEN )
                   CALL EXIT ( 1 )
              END IF
            ELSE IF ( TOKEN .EQ. 'EQUAL_EFF_FREQ' ) THEN
              FL_EQUAL_EFF_FREQ = .TRUE.
            ELSE IF ( TOKEN .EQ. 'REWEI' ) THEN
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( TOKEN .NE. 'SCALE' ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4341, IUER, 'GARC', 'Error in '// &
     &                 'parsing REWEI keyword: SCALE was expected but '// &
     &                 'got '//TOKEN )
                   CALL EXIT ( 1 )
              END IF
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              READ ( UNIT=TOKEN, FMT='(F10.5)', IOSTAT=IER ) SESS_REWEI_SCALE
              IF ( IER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4342, IUER, 'GARC', 'Error in '// &
     &                 'parsing SESS_REWEI_SCALE skale keyword: a real '// &
     &                 'number for scale was expected, but got '//TOKEN )
                   CALL EXIT ( 1 )
              END IF
              IF ( TOKEN .NE. 'ADD_QUADR' ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4343, IUER, 'GARC', 'Error in '// &
     &                 'parsing REWEI keyword: SCALE was expected but '// &
     &                 'got '//TOKEN )
                   CALL EXIT ( 1 )
              END IF
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              READ ( UNIT=TOKEN, FMT='(F10.5)', IOSTAT=IER ) SESS_REWEI_QUADR
              IF ( IER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4344, IUER, 'GARC', 'Error in '// &
     &                 'parsing SESS_REWEI_QUADR skale keyword: a real '//&
     &                 'number for additive parameter was expected, but got '// &
     &                 TOKEN )
                   CALL EXIT ( 1 )
              END IF
            ELSE IF ( TOKEN .EQ. 'XYU' ) THEN
!
! --------- Next 9 lines are for compatibility with the obsolete syntax
!
!
! =========== XYU
!
              EOP_SUPR = TOKEN(1:3)//'YYY'
            ELSE IF ( TOKEN .EQ. 'XY-' ) THEN
!
! =========== XY-
!
              EOP_SUPR = TOKEN(1:3)//'YYY'
            ELSE IF ( TOKEN .EQ. 'X-U' ) THEN
!
! =========== X-U
!
              EOP_SUPR = TOKEN(1:3)//'YYY'
            ELSE IF ( TOKEN .EQ. 'X--' ) THEN
!
! =========== X--
!
              EOP_SUPR = TOKEN(1:3)//'YYY'
            ELSE IF ( TOKEN .EQ. '-YU' ) THEN
!
! =========== -YU
!
              EOP_SUPR = TOKEN(1:3)//'YYY'
            ELSE IF ( TOKEN .EQ. '-Y-' ) THEN
!
! =========== -Y-
!
              EOP_SUPR = TOKEN(1:3)//'YYY'
            ELSE IF ( TOKEN .EQ. '--U' ) THEN
!
! =========== --U
!
              EOP_SUPR = TOKEN(1:3)//'YYY'
            ELSE IF ( TOKEN .EQ. '---' ) THEN
!
! =========== ---
!
              EOP_SUPR = TOKEN(1:3)//'YYY'
            ELSE 
              CALL FERR ( INT2(9124), 'BATCH(garc) Error in parsing '// &
     &            ' a session-line: for database '//DBNAME//'. '// &
     &            ' Unknown keyword '//TOKEN(1:I_LEN(TOKEN))//' was found', &
     &            INT2(0), INT2(0) )
         END IF
 410  CONTINUE 
 810  CONTINUE 
!
      CALL SBIT ( PRE_IP(3), INT2(4), INT2(1) )
      IF ( SCNOUT .EQ. 'Y' ) CALL SBIT ( PRE_IP(3), INT2(4), INT2(0) )
      CALL SBIT ( PRE_IP(3), INT2(1), INT2(1) )
      IF ( USER_PROG .EQ. ' '  .OR.  USER_PROG .EQ. 'NONE' ) THEN
           CALL SBIT ( PRE_IP(3), INT2(11), INT2(1) )
           IF ( KBIT( TEST_FIELD, INT2(1) ) ) THEN
                CALL SBIT ( PRE_IP(3), INT2(11), INT2(0) )
           END IF
      ENDIF
      IF ( WEIGHTS_MODE .EQ. 'A'  .OR.  WEIGHTS_MODE .EQ. 'M' ) THEN
           CALL SBIT ( PRE_IP(3), INT2(11), INT2(0) )
      ENDIF
!
      IF ( ILEN(ADDW_FILE) > 0 .AND. ADDW_USE == ADDW__UNDF ) THEN
           CALL ERR_LOG ( 4345, IUER, 'GARC', 'Keword ADDW_USE '// &
     &         'was not found in the session option line. Since '// &
     &         'ADDW_FILE is specified, this option is mandatory. '// &
     &         'Suporteed values of ADDW_USE: QUAD, REPL, MIN, MAX' )
           CALL FERR ( INT2(9014), 'BATCH(garc) Error in '// &
     &            'processing options for database '//DBNAME, INT2(0), &
     &             INT2(0) )
           CALL EXIT ( 1 ) 
      END IF
!
      IF ( TRAIN ) THEN
           IF ( STRING_SAVE(1:4) == 'GVF ' ) THEN
!
! ------------- Read the GVF file. 
!
                STRING = STRING_SAVE
                CALL SPLITSTRING ( STRING, TOKEN, STRING )
                CALL SPLITSTRING ( STRING, TOKEN, STRING )
!
                IF ( VTD_STATUS == VTD__INIT  .OR. &
     &               VTD_STATUS == VTD__COOR  .OR. &
     &               VTD_STATUS == VTD__STRT  .OR. &
     &               VTD_STATUS == VTD__LOAD  .OR. &
     &               VTD_STATUS == VTD__ALLC  .OR. &
     &               VTD_STATUS == VTD__NOAV       ) THEN
!
                     CONTINUE 
                  ELSE 
                     IUER = -1
                     CALL ERR_LOG ( 4346, IUER, 'GARC', 'Trap of internal '// &
     &                   'control: VTD_CONF should be defined in $MAPPING '// &
     &                   'section for enabling reading GVF databases' )
                     CALL EXIT ( 1 )
                END IF
!
                IF ( ILEN(VTD_CONF_SES) == 0 ) VTD_CONF_SES = VTD_CONF_GLB
                IF ( ILEN(VTD_CONF_SES) == 0 ) VTD_CONF_SES = VTD_CONF_FILE
!
! ------------- Extract from the TOKEN the database name and its version
!
                IP = LINDEX ( TOKEN, '/' )
                STR16 = TOKEN(IP+1:)
                DBNAME = STR16(1:10)
                CALL CHIN ( STR16(13:15), VER_I4 )
                VER = VER_I4
!
                FL_COMP_THEO = .FALSE.
!
! ------------- Read the database in the GVF format
!
                CALL GETDB_DO ( VCAT, GVH, %VAL(VTD_ADR), TOKEN, &
     &                          TRUE__L4, FL_COMP_THEO, FL_SOU_USE_DB_IGNORE, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 4347, IUER, 'GARC', 'Error in attempt to '// &
     &                   'read the database file '//DBNAME(1:I_LEN(DBNAME))// &
     &                   ' while parsing the line '// &
     &                    STRING_SAVE(1:I_LEN(STRING_SAVE))// &
     &                   ' of the control file' )
                     CALL EXIT ( 1 )
                END IF
                CALL CLOSENAMFIL()
                CALL OPENNAMFIL()
!
                PRE_IP(4) = 1
              ELSE 
!
! ------------- Schedule GTSUP
!
                CALL USE_BUFFER ( IDBNAM, INT2(6), 'OWC' )
                CALL RUN_PROG   ( 'GTSUP', 'WAIT', INT2(0) )
                IF ( PRE_IP(4) .EQ. -1 ) THEN
                     WRITE ( ERRSTR, 8888 ) (IDBNAM(I),I=1,5), VER
                     IF ( SOLTYP .EQ. 'I' ) THEN
                          WRITE(23,8888) (IDBNAM(I),I=1,5), VER
 8888                     FORMAT ( 'Superfile for ',5A2,' ver. ',I2,' not found' )
                        ELSE
                          CALL FERR ( INT2(9126), ERRSTR, INT2(0), INT2(0) )
                    ENDIF
                ENDIF
           ENDIF
         ELSE
!
! -------- Check: was the session found?
!
           IP = INDEX ( STRING_SAVE, '! Not found' )
           IF ( IP .GT. 0 ) THEN
!
! ------------- File was not found. Skip the arc
!
                IF ( G_WARNING ) THEN
                     WRITE (  6, '(A)' ) 'Warning: superfile for database '// &
     &                        STRING_SAVE(1:I_LEN(STRING_SAVE))//' -- Skipped'
                     WRITE ( 23, '(A)' ) 'Warning: superfile for database '// &
     &                        STRING_SAVE(1:I_LEN(STRING_SAVE))//' -- Skipped'
                END IF
                RETURN ! go out
           END IF
!
           IF ( STRING_SAVE(1:4) == 'GVF ' ) THEN
                STRING = STRING_SAVE
                CALL SPLITSTRING ( STRING, TOKEN, STRING )
                CALL SPLITSTRING ( STRING, TOKEN, STRING )
                CALL CLRCH ( VCAT_REPO )
                DO 450 J5=1,VCAT%NREPS
                   IL = ILEN(VCAT%GVF_ENV_DIR(J5))
                   IF ( IL > LEN(TOKEN) ) THEN
                        IL = LEN(TOKEN)
                   END IF
                   IF ( TOKEN(1:IL) == VCAT%GVF_ENV_DIR(J5)(1:IL) ) THEN
                        VCAT_REPO = VCAT%GVF_REP_NAME(J5)
                   END IF
 450            CONTINUE 
                IF ( ILEN(VCAT_REPO) == 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 4348, IUER, 'GARC', 'Trap of internal '// &
     &                   'control: cannot determine VCAT_REPO for database '// &
     &                    TOKEN )
                     CALL EXIT ( 1 )
                END IF
!
                IF ( VTD_STATUS == VTD__INIT  .OR. &
     &               VTD_STATUS == VTD__COOR  .OR. &
     &               VTD_STATUS == VTD__STRT  .OR. &
     &               VTD_STATUS == VTD__LOAD  .OR. &
     &               VTD_STATUS == VTD__ALLC  .OR. &
     &               VTD_STATUS == VTD__NOAV       ) THEN
!
                     CONTINUE 
                  ELSE 
                     IUER = -1
                     CALL ERR_LOG ( 4349, IUER, 'GARC', 'Trap of internal '// &
     &                   'control: VTD_CONF should be defined in $MAPPING '// &
     &                   'section for enabling reading GVF databases' )
                     CALL EXIT ( 1 )
                END IF
!
                IF ( ILEN(VTD_CONF_SES) == 0 ) VTD_CONF_SES = VTD_CONF_GLB
                IF ( ILEN(VTD_CONF_SES) == 0 ) VTD_CONF_SES = VTD_CONF_FILE
                CALL USE_GLBFIL_4  ( 'OWC' )
!
! ------------- Extract from the TOKEN the database name and its version
!
                IP = LINDEX ( TOKEN, '/' )
                STR16 = TOKEN(IP+1:)
                DBNAME = STR16(1:10)
                CALL CHIN ( STR16(13:15), VER_I4 )
                VER = VER_I4
!
                FL_COMP_THEO = .FALSE.
!@
!@                IF ( WEIGHT_ALGORITHM .EQ. WEIGHT__UPWEI      .OR. &
!@     &               WEIGHT_ALGORITHM .EQ. WEIGHT__UPWEI_OPT  .OR. &
!@     &               WEIGHT_ALGORITHM .EQ. WEIGHT__ELIM            ) THEN
!@!
!@                     FL_COMP_THEO = .FALSE.
!@                   ELSE 
!@                     FL_COMP_THEO = .TRUE.
!@                END IF
!
                IF ( USER_PROG(1:1) .NE. ' '    .AND. &
     &               USER_PROG(1:4) .NE. 'NONE'       ) THEN
                     FL_COMP_THEO = .TRUE.
                END IF
!
                IUER = -1
                CALL GETDB_DO ( VCAT, GVH, %VAL(VTD_ADR), DBNAME, TOKEN, &
     &                          TRUE__L4, FL_COMP_THEO, FL_SOU_USE_DB_IGNORE, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 4350, IUER, 'GARC', 'Error in attempt to '// &
     &                   'read the database file '//DBNAME(1:I_LEN(DBNAME))// &
     &                   ' while parsing the line '// &
     &                    STRING_SAVE(1:I_LEN(STRING_SAVE))// &
     &                   ' of the control file' )
                     CALL EXIT ( 1 )
                END IF
                CALL CLOSENAMFIL()
                CALL OPENNAMFIL()
!
                PRE_IP(4) = 1
              ELSE
                IUER = -1
                CALL ERR_LOG ( 4351, IUER, 'GARC', 'Error parsing '// &
     &              'the line '//STRING_SAVE(1:I_LEN(STRING_SAVE))// &
     &              ' of the control file -- it does not specify '// &
     &              ' a valid GVH file' )
                CALL EXIT ( 1 )
           END IF
           PRE_IBATCH = PRE_IP(3)
           KSCREEN = .FALSE.
      END IF
!
! --- Preparing the line with database name and version number. We put it
! --- in variable DBNAME in ba2cm.i  It will be useful for generating error
! --- mesages
!
      CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
      CALL CLRCH ( DBNAME_MES )
      DBNAME_MES = CDBNAM(1)
      DBNAME_MES(12:) = '<'
      CALL INCH ( INT4 ( IDBV(1) ), DBNAME_MES(13:) )
      DBNAME_MES( I_LEN(DBNAME_MES)+1: ) = '>'
!
      RETURN
      END  SUBROUTINE  GARC  !#!#
