      SUBROUTINE WRITE_SINEX ( FL_GLOBAL, CNSTROBJ, COV_MAT, EST_VEC, SIG_VEC, &
     &                         APR_VAL, APR_SIG, EST_VAL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  WRITE_SINEX generates listing of the solution in SINEX    *
! *   format. The set of variables xx_SINEX_xx kept in glbc4 controls    *
! *   the work of this routine.                                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  FL_GLOBAL ( LOGICAL*2 ) -- Flag: TRUE if the listing of global      *
! *                             parameters is to be generated. False     *
! *                             if the listing should contain only local *
! *                             (session-dependent parameters).          *
! *  CNSTROBJ  ( RECORD    ) -- The data structure with information      *
! *                             about equations of constraints.          *
! *  COV_MAT   ( REAL*8    ) -- Full covariance matrix.                  *
! *  EST_VEC   ( REAL*8    ) -- Vector of the adjustments.               *
! *  SIG_VEC   ( REAL*8    ) -- Vector of the formal uncertainties of    *
! *                             the parameter estimates.                 *
! *  APR_VAL   ( REAL*8    ) -- Vector of apriori values of some         *
! *                             parameters. Dimension: N__SNX. Meaning   *
! *                             of each element is defined in solve.i    *
! *                             Current version (2002.05.03) assumes     *
! *                             that only elements which correspond to   *
! *                             EOP are defined.                         *
! *  APR_SIG   ( REAL*8    ) -- Vector of apriori sigmas of the          *
! *                             parameters. Dimension: N__SNX. Meaning   *
! *                             of each element is defined in solve.i    *
! *  EST_VAL   ( REAL*8    ) -- Vector of total values of some           *
! *                             parameters. Dimension: N__SNX. Meaning   *
! *                             of each element is defined in solve.i    *
! *                             Current version (2002.05.03) assumes     *
! *                             that only elements which correspond to   *
! *                             EOP are defined.                         *
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
! *  WHEN        WHO  WHAT                                               *
! *  2002.03.19  pet  Beginning of work                                  *
! *  2002.05.03  pet  Release of version 1.0 .                           *
! *  2002.05.09  pet  Added two new blocks: DECOMPOSED_NORMAL_MATRIX and *
! *                   DECOMPOSED_NORMAL_VECTOR.                          *
! *  2002.05.30  pet  Fixed the bug: the previous version did not        *
! *                   recognize the stations with episodic motion.       *
! *  2002.10.01  pet  Upgraded for support of solutions made in global   *
! *                   mode. Added support of the folowing parameters:    *
! *                   source coordinates, source proper motion, antenna  *
! *                   axis offset, site velocity.                        *
! *  2002.12.24  pet  Replaced variable TIME0 and constant PRM_YR_REF    *
! *                   with variables SIT_EST_EPOCH, SRC_EST_EPOCH.       *
! *  2002.12.24  pet  Added support the case when the reference epoch of *
! *                   site position apriori and site position estimates  *
! *                   is not the same.                                   *
! *  2003.04.22  pet  1) Added a new feature: format description is      *
! *                      added   to the comment section just after       *
! *                      contents of the comment file. The format        *
! *                      description is copied from the file             *
! *                      $PSOLVE_ROOT/doc/sinex.txt (if this file exists,*
! *                      of course).                                     *
! *                    2) Changed the logic of treatment the station     *
! *                       whose positions are modeled by a linear model  *
! *                       with discontinuity(ies). The current version   *
! *                       always uses point code A. Field SBIN           *
! *                       ( sub-index ) is used for ditinguishing        *
! *                       parameters for site positions before and       *
! *                       after discontinuity(ies). SBIN is initialized  *
! *                       to 1. Parameter of the station position before *
! *                       a discontinutiy has SBIN equal to 1. After     *
! *                       the discontinutiy SBIN is incremented.         *
! *  2003.07.07  pet  Fixed a bug: RD_TEXT required integer*4 argument   *
! *                   M_GPA instead of integer*2 argument M_GPA.         *
! *  2003.08.18  pet  Renamed SITE/EPOCH to SOLUTION/EPOCH.              *
! *  2005.03.01  pet  Added new feature: WRITE_SINEX writes down control *
! *                   file into the the listing.                         *
! *  2008.04.29  pet  Added support of 2.20 Sinex format.                *
! *                                                                      *
! *  ### 19-MAR-2002  WRITE_SINEX  v2.6  (c) L. Petrov  29-APR-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'astro_constants.i'
      INCLUDE    'solve.i'
      INCLUDE    'precm.i'
      INCLUDE    'prfil.i'
      INCLUDE    'erm.i'
      INCLUDE    'socom.i'
      INCLUDE    'socom_plus.i'
      INCLUDE    'oborg.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'glbcm.i'
      INCLUDE    'cnstr.i'
      INCLUDE    'param.i'
      INCLUDE    'bindisp.i'
      INCLUDE    'flyby.i'
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      CHARACTER  ROUTINE__LABEL*40
      PARAMETER  ( ROUTINE__LABEL = 'WRITE_SINEX, revision date 2008.07.25   ' )
!
      LOGICAL*2  FL_GLOBAL
      REAL*8     COV_MAT(*), EST_VEC(*), SIG_VEC(*)
      REAL*8     APR_VAL(N__SNX), APR_SIG(N__SNX), EST_VAL(N__SNX)
      INTEGER*4  IUER
!
      INTEGER*4  M_STA, M_SRC, M_BUF, M_ECC, MIND
      PARAMETER  ( M_STA = MAX_STA )
      PARAMETER  ( M_SRC = MAX_SRC )
      PARAMETER  ( M_BUF = MAX_STA )
      PARAMETER  ( M_ECC = 1024    )
      PARAMETER  ( MIND  = 32 )
      INTEGER*8  MEM_LEN
      ADDRESS__TYPE :: MEM_ADR, &
     &           ADR_NOR_DCM, ADR_NRV_DCM, ADR_CNS_MAT, ADR_CNS_WMA, &
     &           ADR_CNS_NRM, ADR_WEI_CNS
      INTEGER*4  LEN_NOR_DCM, LEN_NRV_DCM, LEN_CNS_MAT, LEN_CNS_WMA, &
     &           LEN_CNS_NRM, LEN_WEI_CNS
      CHARACTER  DB_NAME*10, YEAR_STR*4, VERS_STR*3, FILOUT*128, &
     &           MASTER_DIR*128, SAVE_DIR*128, SESS_CODE*128, &
     &           STR_PT_NUM(M_GPA)*4
      CHARACTER  DATE_BEG*23, DATE_END*23, DATE_MID*23, DATE_NOW*12, &
     &           STR_PAR*5, PAR_CODE*5, SOURCE_ID(M_SRC)*4, OUT*128, &
     &           STA_NAME(M_STA)*8, STA_CODE(M_STA)*2, STA_DOME(M_STA)*9, &
     &           STA_DESC(M_STA)*80, &
     &           IVS_NAME(M_SRC)*8, IERS_NAME(M_SRC)*8, ICRF_NAME(M_SRC)*16, &
     &           J2000_NAME(M_SRC)*10, DATE_STA*23, DATE_SRC*23
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128, &
     &           SYSNAME*128, NODENAME*128, HARDWARE*128, REVISION_STR*16, &
     &           FINAM*128, STR*128, LONG_STR*16, LATT_STR*16
      CHARACTER  STA_DATE_BEG(M_STA)*12, STA_DATE_END(M_STA)*12, &
     &           STA_DATE_MID(M_STA)*12, &
     &           SRC_DATE_BEG(M_SRC)*12, SRC_DATE_END(M_SRC)*12, &
     &           SRC_DATE_MID(M_SRC)*12
      CHARACTER  LPARM(M_GPA)*20, LPARM_LOC(M_GPA)*20, LPARM_GLO(M_GPA)*20
      CHARACTER  INCBUF(M_BUF)*64, EXCBUF(M_BUF)*64, SINDATE_MID*12, &
     &           SINDATE_EOP*12, SINDATE_STA*12, SINDATE_SRC*12, &
     &           SAPDATE_EOP*12, SAPDATE_STA*12, SAPDATE_SRC*12, &
     &           ECCDAT_FILE*128, BUF_ECC(M_ECC)*128, PT_DAT(M_STA)*6, &
     &           STA_NAM_CLEAN*8, REG*3
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      REAL*8     LATITUDE, LONGITUDE, HEIGHT, FJD_BEG, FJD_END
      REAL*8     ADJ_VEC(M_GPA)
      REAL*8     SMALL_EPS, FJD_ECC_MIN, FJD_ECC_MAX, FJD_ECC, SEC
      PARAMETER  ( SMALL_EPS = 1.D-14 )
      LOGICAL*4  LEX
      INTEGER*4  DB_VERS, MJD, LUN, LOUT, IOS, IYEAR, STA_CDP(M_STA), L_SRC, &
     &           L_STA, ISTA, ISRC, ISTN, L_SPR, ICMP, IEPOCH, ICDP_REPL, &
     &           STA_PARIND(M_STA), PAR_PT_NUM(M_GPA), J1, J2, J3, J4, J5, &
     &           J6, J7, J8, J9, J10, J11, J12, J13, J14, J15, J16, J17, LIND, &
     &           IND(2,MIND), IER
      ADDRESS__TYPE :: DIR_DESC
      INTEGER*2  IX_ALL_GLO(M_GPA)
      CHARACTER  SED_FILE*128, COM_STR*512
      INTEGER*4  IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10, IL, IP, &
     &           N_INC, N_EXC, N_ECC, INC_IND, EXC_IND, L_PAR, MBUF, NBUF, &
     &           IND_SPR(M_GPA), TYP_SPR(M_GPA), UNIX_DATE, IS, ISIG, ICOD
      INTEGER*8  SIZE_I8 
      INTEGER*2  NPARM, NPARM_LOC, NPARM_GLO, IM_I2, ID_I2, IY_I2, IT_I2
      LOGICAL*2  L2_TRUE, L2_FALSE
      PARAMETER  ( L2_TRUE  = .TRUE.  )
      PARAMETER  ( L2_FALSE = .FALSE. )
      CHARACTER, ALLOCATABLE :: BUF(:)*256
      CHARACTER     SINEX_FORMAT_DESCR_FILE*128, SINEX_220_SED*32
      DATA          SINEX_FORMAT_DESCR_FILE / 'sinex.txt'     /
      DATA          SINEX_220_SED           / 'sinex_220.sed' /
!
      CHARACTER  CHAR26*2
      INTEGER*4  I, J
      INTEGER*8  LOCC
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: GET_DBVERS, GET_UNIT, ILEN, I_LEN, LTM_DIF, &
     &                       SYSTEM
      ADDRESS__TYPE, EXTERNAL :: OPENDIR
      LOGICAL*4, EXTERNAL ::  CHECK_STABIT
      CHARACTER, EXTERNAL ::  GET_CDATE*19, JD_TO_DATE*23, GET_DBNAME*10, &
     &           UNIX_DATE_TO_DATE*19
      LOCC(I,J) = INT8(MIN(I,J)) + (INT8(MAX(I,J))*INT8(MAX(I,J)-1))/2
!
! --- Get the minimal and maximal dates in the eccentricity file.
! --- Date before FJD_ECC_MIN means "always"  (permanent station)
! --- Date After  FJD_ECC_MAX means "forever" (permanent station)
!
      CALL DATE_TO_TIME ( '1970.01.02-00:00:00.0', MJD, SEC, 0 )
      FJD_ECC_MIN = J2000__JD + (MJD - J2000__MJD) + SEC/86400.0
!
      CALL DATE_TO_TIME ( '2049.12.30-00:00:00.0', MJD, SEC, 0 )
      FJD_ECC_MAX = J2000__JD + (MJD - J2000__MJD) + SEC/86400.0
!
! --- Check whether the subroutine should be running in this mode (local/global)
!
      IF ( FL_GLOBAL .AND. .NOT. FL_SINEX_GLO ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
      IF ( .NOT. FL_GLOBAL .AND. .NOT. FL_SINEX_LOC ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( SINEX_VERS(1:4) .NE. '2.10'  .AND. &
     &     SINEX_VERS(1:4) .NE. '2.20'        ) THEN
           CALL ERR_LOG ( 3711, IUER, 'WRITE_SINEX', 'I am sorry to tell you '// &
     &         'a terrible news: Sinex format '// &
     &          SINEX_VERS(1:I_LEN(SINEX_VERS))//' is not supported. '// &
     &         'Refer to documenation to learn which Sinex format is '// &
     &         'currently supported' )
           RETURN
      END IF
!
! --- Get directory for master files
!
!
      CALL GETENVAR ( 'MASTER_DIR', MASTER_DIR )
      IF ( ILEN(MASTER_DIR) == 0 ) THEN
           MASTER_DIR = SOLVE_SAVE_DIR//'/master_dir'
           DIR_DESC = OPENDIR ( MASTER_DIR(1:I_LEN(MASTER_DIR))//CHAR(0) )
           IF ( DIR_DESC .LE. 0 ) THEN
                CALL ERR_LOG ( 3712, IUER, 'WRITE_SINEX', 'MASTER_DIR '// &
     &              'directory '//MASTER_DIR(1:I_LEN(MASTER_DIR))//' has not '// &
     &              'been defined. Please correct this error and either '// &
     &              're-compole Calc/Solve or defined environoment '// &
     &              'variable MASTER_DIR' )
                RETURN 
             ELSE 
               CALL CLOSEDIR ( %VAL(DIR_DESC) )
           END IF
         ELSE
           DIR_DESC = OPENDIR ( MASTER_DIR(1:I_LEN(MASTER_DIR))//CHAR(0) )
           IF ( DIR_DESC .LE. 0 ) THEN
                CALL ERR_LOG ( 3713, IUER, 'WRITE_SINEX', 'Environment '// &
     &              'variable MASTER_DIR points to the directory '// &
     &               MASTER_DIR(1:I_LEN(MASTER_DIR))//' that does not '// &
     &               'exist. Please correct this error' )
                RETURN 
             ELSE 
               CALL CLOSEDIR ( %VAL(DIR_DESC) )
           END IF
      END IF
      IF ( MASTER_DIR(I_LEN(MASTER_DIR):I_LEN(MASTER_DIR)) .NE. '/' ) THEN
           MASTER_DIR(I_LEN(MASTER_DIR)+1:I_LEN(MASTER_DIR)+1) =    '/'
      END IF
!
! --- Get database name and version number
!
      DB_NAME = GET_DBNAME()
      DB_VERS = GET_DBVERS()
!
! --- Resolve database name and get session code
!
      CALL ERR_PASS ( IUER, IER )
      IF ( DB_NAME(1:1) == '$' ) THEN
           CALL RESOLVE_DBNAME ( MASTER_DIR, DB_NAME,  SESS_CODE, IER )
         ELSE 
           CALL RESOLVE_DBNAME ( MASTER_DIR, MK3_DBNM, SESS_CODE, IER )
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3714, IUER, 'WRITE_SINEX', 'Error in an '// &
     &         'attempt to build sinex filename: database name '// &
     &         'name '//DB_NAME//' was npt resolved to a session '// &
     &         'code' )
           RETURN
      END IF
      CALL TRAN ( 12, SESS_CODE, SESS_CODE ) ! Transform the name to lower reg.
!
! --- If the database name ends on " ", replace it with "_"
!
      IF ( DB_NAME(10:10) .EQ. ' ' ) DB_NAME(10:10) = '_'
!
! --- Format database version number
!
      CALL CLRCH  ( VERS_STR )
      CALL INCH   ( DB_VERS, VERS_STR )
      CALL CHASHR ( VERS_STR )
      CALL BLANK_TO_ZERO ( VERS_STR )
!
! --- Build the name of the output Sinex file
!
      CALL CLRCH ( FILOUT )
      FILOUT = SINEX_OUTFILE
!
! --- The filename specified in batch Solve can have different macros which
! --- are to be expanded into a substring whcih became a part of the filename.
! --- We will chack now for pattern and expand it one by one and correct
!
      IL = ILEN(FILOUT)
      DO 410 J1=1,IL
!
! ------ Find position of a pattern (0 means no pattern is found) in the
! ------ filename.
!
         IP1  = INDEX ( FILOUT, '<YY>'        )
         IP2  = INDEX ( FILOUT, '<YYYY>'      )
         IP3  = INDEX ( FILOUT, '<DATABASE>'  )
         IP4  = INDEX ( FILOUT, '<VERS>'      )
         IP5  = INDEX ( FILOUT, '<WORK_DIR>'  )
         IP6  = INDEX ( FILOUT, '<SAVE_DIR>'  )
         IP7  = INDEX ( FILOUT, '<PSOLVE_ROOT>'  )
         IP8  = INDEX ( FILOUT, '<SPOOL_DIR>' )
         IP9  = INDEX ( FILOUT, '<ID>'        )
         IP10 = INDEX ( FILOUT, '<SESSION>'   )
!
         IF ( IP1 .GT. 0 ) THEN
!
! ----------- <YY>  (2-digit year number)
!
              IF ( IP1 .EQ. 1 ) THEN
                   FILOUT = DB_NAME(2:3)//FILOUT(IP1+4:)
                 ELSE
                   FILOUT = FILOUT(1:IP1-1)//DB_NAME(2:3)//FILOUT(IP1+4:)
              END IF
              GOTO 410
           ELSE IF ( IP2 .GT. 0 ) THEN
!
! ----------- <YYYY> (4-digit year number)
!
              CALL CHIN ( DB_NAME(2:3), IYEAR )
              IF ( IYEAR .GE. 70 ) THEN
                   YEAR_STR = '19'//DB_NAME(2:3)
                 ELSE
                   YEAR_STR = '20'//DB_NAME(2:3)
              END IF
              IF ( IP2 .EQ. 1 ) THEN
                   FILOUT = YEAR_STR//FILOUT(IP2+6:)
                 ELSE
                   FILOUT = FILOUT(1:IP2-1)//YEAR_STR//FILOUT(IP2+6:)
              END IF
              GOTO 410
           ELSE IF ( IP3 .GT. 0 ) THEN
!
! ----------- <DATABASE> (database name)
!
              IF ( IP3 .EQ. 1 ) THEN
                   FILOUT = DB_NAME(2:10)//FILOUT(IP3+10:)
                 ELSE
                   FILOUT = FILOUT(1:IP3-1)//DB_NAME(2:10)//FILOUT(IP3+10:)
              END IF
              GOTO 410
           ELSE IF ( IP4 .GT. 0 ) THEN
!
! ----------- <VERS> (three digit version number with leading zeroes)
!
              IF ( IP4 .EQ. 1 ) THEN
                   FILOUT = VERS_STR//FILOUT(IP4+6:)
                 ELSE
                   FILOUT = FILOUT(1:IP4-1)//VERS_STR//FILOUT(IP4+6:)
              END IF
              GOTO 410
           ELSE IF ( IP5 .GT. 0 ) THEN
!
! ----------- <WORK_DIR> (working directory with trailing slash character)
!
              IF ( IP5 .EQ. 1 ) THEN
                   FILOUT = PRE_SCR_DIR(1:PRE_SD_LEN-1)//FILOUT(IP5+10:)
                 ELSE
                   FILOUT = FILOUT(1:IP5-1)//PRE_SCR_DIR(1:PRE_SD_LEN-1)// &
     &                      FILOUT(IP5+10:)
              END IF
              GOTO 410
           ELSE IF ( IP6 .GT. 0 ) THEN
!
! ----------- <SAVE_DIR> (Solve save directory name with trailing slash character)
!
              IF ( IP6 .EQ. 1 ) THEN
                   FILOUT = PRE_SAV_DIR(1:PRE_SV_LEN-1)//FILOUT(IP6+10:)
                 ELSE
                   FILOUT = FILOUT(1:IP6-1)//PRE_SAV_DIR(1:PRE_SV_LEN-1)// &
     &                      FILOUT(IP6+10:)
              END IF
              GOTO 410
           ELSE IF ( IP7 .GT. 0 ) THEN
!
! ----------- <PSOLVE_ROOT> (VTD/Post-Solve root directory without trailing slash)
!
              IF ( IP7 .EQ. 1 ) THEN
                   FILOUT = PRE_ROOT_DIR(1:PRE_ROOT_LEN)//FILOUT(IP7+12:)
                 ELSE
                   FILOUT = FILOUT(1:IP7-1)//PRE_ROOT_DIR(1:PRE_ROOT_LEN)// &
     &                      FILOUT(IP7+12:)
              END IF
              GOTO 410
           ELSE IF ( IP8 .GT. 0 ) THEN
!
! ----------- <SPOOL_DIR> (Solve spool directory with trailing slash character)
!
              IF ( IP8 .EQ. 1 ) THEN
                   FILOUT = PRE_SPL_NAM(1:PRE_SPL_LEN-1)//FILOUT(IP8+10:)
                 ELSE
                   FILOUT = FILOUT(1:IP8-1)//PRE_SPL_NAM(1:PRE_SPL_LEN-1)// &
     &                      FILOUT(IP8+10:)
              END IF
              GOTO 410
           ELSE IF ( IP9 .GT. 0 ) THEN
!
! ----------- <ID> (Solution ID specified in Solve batch file)
!
              IF ( IP9 .EQ. 1 ) THEN
                   FILOUT = SOLUID_CHR(1:I_LEN(SOLUID_CHR))//FILOUT(IP9+10:)
                 ELSE
                   FILOUT = FILOUT(1:IP9-1)//SOLUID_CHR(1:I_LEN(SOLUID_CHR))// &
     &                     FILOUT(IP9+10:)
              END IF
              GOTO 410
           ELSE IF ( IP10 .GT. 0 ) THEN
!
! ----------- <SESSION> (Session name in lower register)
!
              IF ( IP10 .EQ. 1 ) THEN
                   FILOUT = SESS_CODE(1:I_LEN(SESS_CODE))//FILOUT(IP10+9:)
                 ELSE
                   FILOUT = FILOUT(1:IP10-1)//SESS_CODE(1:I_LEN(SESS_CODE))// &
     &                      FILOUT(IP10+9:)
              END IF
              GOTO 410
         END IF
         GOTO 810
 410  CONTINUE
 810  CONTINUE
      SINEX_REALNAME = FILOUT
      CALL USE_GLBFIL_4 ( 'OWC' )
!
! --- Get parameter names as character*20 strings and put them in array LPARM
! --- This list will contain all parameters: local and global
!
      KGLOBONLY = .FALSE.
      CALL GET_NAMES ( LPARM, INT2(20), M_GPA, NPARM, L2_TRUE, L2_FALSE )
      L_PAR = NPARM
!
      IF ( FL_GLOBAL ) THEN
!
! -------- Make a copy of LPARM
!
           CALL LIB$MOVC3 ( 20*L_PAR, LPARM, LPARM_LOC )
           KGLOBONLY = .TRUE.
           CALL GET_NAMES ( LPARM, INT2(20), M_GPA, NPARM, L2_TRUE, L2_TRUE )
!
! -------- First we have to get a list of global parameters.
! -------- Read part of GLB  -file where information about global
! -------- parameters is kept
!
           CALL USE_GLBFIL_2 ( 'ORC' )
!
! -------- Setting flag for further call of GET_NAMES which will point out
! -------- on neccesity to build list of only ghlobal parameters
!
           CALL DEPAR()
!
! -------- Building a list of global parameters -- LPARM_GLO
!
           KGLOBONLY = .TRUE.
           CALL GET_NAMES ( LPARM_GLO, INT2(20), M_GPA, NPARM_GLO, L2_TRUE, &
     &                      L2_TRUE )
           KGLOBONLY = .FALSE.
!
! -------- Make a corsss reference table form all parameters to global
!
           CALL CXEPAR_OPT20 ( LPARM, IX_ALL_GLO, NPARM, LPARM_GLO, NPARM_GLO )
!
! -------- Now "update" the list LPARM_LOC -- it will have empty lines for
! -------- local parameters. Thus, global parameters will be erased, and
! -------- cnstr will not be in  a position to set constraints on them.
! -------- A cunning trick, isn't it?
!
           DO 420 J2=1,NPARM
              IF ( IX_ALL_GLO(J2) .GT. 0 ) THEN
                   LPARM_LOC(J2) = '                    '
              END IF
 420       CONTINUE
         ELSE
!
! -------- No global parameters
!
           NPARM_GLO = 0
           NPARM_LOC = NPARM
      END IF
!
! --- Check: does SINEX_INCLUDE file exist
!
      INQUIRE ( FILE=SINEX_INCLUDE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 3715, IUER, 'WRITE_SINEX', 'SINEX_INCLUDE file '// &
     &          SINEX_INCLUDE(1:I_LEN(SINEX_INCLUDE))//' was not found' )
           RETURN
      END IF
!
! --- Read Sinex include file. This file contains the list of parameters
! --- which are to be included in the Sinex listing. Parameters can have
! --- a wildcard symbol * or ?
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( SINEX_INCLUDE, M_GPA, INCBUF, N_INC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3716, IUER, 'WRITE_SINEX', 'Error in an '// &
     &         'attempt to read SINEX_INCLUDE file '//SINEX_INCLUDE )
           RETURN
      END IF
!
! --- Check: does SINEX_EXCLUDE file exist
!
      INQUIRE ( FILE=SINEX_EXCLUDE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 3717, IUER, 'WRITE_SINEX', 'SINEX_EXCLUDE: '// &
     &          SINEX_EXCLUDE(1:I_LEN(SINEX_EXCLUDE))//' was not found' )
           RETURN
      END IF
!
! --- Read SINEX_EXCFIL file. This file contains names of the parameters which
! --- are to be not included in the sinex listing
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( SINEX_EXCLUDE, M_GPA, EXCBUF, N_EXC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3718, IUER, 'WRITE_SINEX', 'Error in an '// &
     &         'attempt to read SINEX_EXCLUDE file '//SINEX_EXCLUDE )
           RETURN
      END IF
!
! --- Now scan parameter list. We will create another list L_SPR, IND_SPR
! --- with indices of the estimated parameters which should be put in SINEX
! --- listing. Array TYP_SPR refers the type of the parameter
!
      L_SPR = 0
      PAR_CODE = '      '
      DO 430 J3=1,L_PAR
!
! ------ Check whether the J3-th parameter is in the INCBUF, EXCBUF lists
!
         INC_IND = LTM_DIF ( 3, N_INC, INCBUF, LPARM(J3) )
         EXC_IND = LTM_DIF ( 3, N_EXC, EXCBUF, LPARM(J3) )
         IF ( INC_IND .LE. 0  .OR. &  ! Not in inc-list or in exc-list
     &        EXC_IND .GT. 0       ) GOTO 430 ! verdict: not to consider it.
         CALL CHIN ( LPARM(J3)(11:16), IEPOCH )
!
         IF ( LPARM(J3)(9:9)   .EQ. ' '    .AND. &
     &        LPARM(J3)(17:20) .EQ. '-COO' .AND. &
     &        IEPOCH .GT. 0 .AND. IEPOCH .LT. 999999 ) THEN
!
              CALL ERR_LOG ( 3719, IUER, 'WRITE_SINEX', 'Trap of internal '// &
     &            'control: station '//LPARM(J3)(1:8)//' was parameterized '// &
     &            'by linear spline. We cannot process such a station in '// &
     &            'Sinex listing. Please update your control file: either '// &
     &            're-parameterize your solution or exclude this parameter '// &
     &            'from the list of parameters brought for Sinex listing' )
              RETURN
         END IF
!
         IF ( LPARM(J3)(12:20) .EQ. 'COMPONENT'          .OR. &
     &        ( LPARM(J3)(9:9)   .EQ. ' '    .AND. &
     &          LPARM(J3)(17:20) .EQ. '-POS' .AND. &
     &          IEPOCH .GT. 0 .AND. IEPOCH .LT. 999999 )       ) THEN
!
! ----------- Station coordinates
!
              L_SPR = L_SPR + 1
              IND_SPR(L_SPR) = J3
              IF ( LPARM(J3)(10:10) .EQ. 'X' ) THEN
                   TYP_SPR(L_SPR) = STX__SNX
                 ELSE IF ( LPARM(J3)(10:10) .EQ. 'Y' ) THEN
                   TYP_SPR(L_SPR) = STY__SNX
                 ELSE IF ( LPARM(J3)(10:10) .EQ. 'Z' ) THEN
                   TYP_SPR(L_SPR) = STZ__SNX
              END IF
              PAR_CODE(5:5) = 'S'
           ELSE IF ( LPARM(J3)(12:20) .EQ. 'VELOCITY ' ) THEN
!
! ----------- Station velocities
!
              L_SPR = L_SPR + 1
              IND_SPR(L_SPR) = J3
              IF ( LPARM(J3)(10:10) .EQ. 'X' ) THEN
                   TYP_SPR(L_SPR) = VEX__SNX
                 ELSE IF ( LPARM(J3)(10:10) .EQ. 'Y' ) THEN
                   TYP_SPR(L_SPR) = VEY__SNX
                 ELSE IF ( LPARM(J3)(10:10) .EQ. 'Z' ) THEN
                   TYP_SPR(L_SPR) = VEZ__SNX
              END IF
              PAR_CODE(5:5) = 'S'
           ELSE IF ( LPARM(J3)(1:10) .EQ. 'X WOBBLE 0' ) THEN
!
! ----------- X pole coordinate
!
              L_SPR = L_SPR + 1
              IND_SPR(L_SPR) = J3
              TYP_SPR(L_SPR) = XPL__SNX
              PAR_CODE(3:3) = 'E'
           ELSE IF ( LPARM(J3)(1:10) .EQ. 'Y WOBBLE 0' ) THEN
!
! ----------- Y pole coordinate
!
              L_SPR = L_SPR + 1
              IND_SPR(L_SPR) = J3
              TYP_SPR(L_SPR) = YPL__SNX
              PAR_CODE(3:3) = 'E'
           ELSE IF ( LPARM(J3)(1:10) .EQ. 'UT1-TAI  0' ) THEN
!
! ----------- UT1 angle
!
              L_SPR = L_SPR + 1
              IND_SPR(L_SPR) = J3
              TYP_SPR(L_SPR) = UT1__SNX
              PAR_CODE(3:3) = 'E'
           ELSE IF ( LPARM(J3)(1:10) .EQ. 'X WOBBLE 1' ) THEN
!
! ----------- X pole rate
!
              L_SPR = L_SPR + 1
              IND_SPR(L_SPR) = J3
              TYP_SPR(L_SPR) = XPR__SNX
              PAR_CODE(3:3) = 'E'
           ELSE IF ( LPARM(J3)(1:10) .EQ. 'Y WOBBLE 1' ) THEN
!
! ----------- Y pole rate
!
              L_SPR = L_SPR + 1
              IND_SPR(L_SPR) = J3
              TYP_SPR(L_SPR) = YPR__SNX
              PAR_CODE(3:3) = 'E'
           ELSE IF ( LPARM(J3)(1:10) .EQ. 'UT1-TAI  1' ) THEN
!
! ----------- UT1 rate
!
              L_SPR = L_SPR + 1
              IND_SPR(L_SPR) = J3
              TYP_SPR(L_SPR) = UTR__SNX
              PAR_CODE(3:3) = 'E'
           ELSE IF ( LPARM(J3)(1:18) .EQ. 'LONGITUDE NUTATION' ) THEN
!
! ----------- Nutation in longitude
!
              L_SPR = L_SPR + 1
              IND_SPR(L_SPR) = J3
              IF ( NUT_USE_CODE == NUT__XY ) THEN
                   TYP_SPR(L_SPR) = NTX__SNX
                 ELSE 
                   TYP_SPR(L_SPR) = PSI__SNX
              END IF
              PAR_CODE(3:3) = 'E'
           ELSE IF ( LPARM(J3)(1:18) .EQ. 'OBLIQUITY NUTATION' ) THEN
!
! ----------- Nutation in obliquity
!
              L_SPR = L_SPR + 1
              IND_SPR(L_SPR) = J3
              IF ( NUT_USE_CODE == NUT__XY ) THEN
                   TYP_SPR(L_SPR) = NTY__SNX
                 ELSE 
                   TYP_SPR(L_SPR) = EPS__SNX
              END IF
              PAR_CODE(3:3) = 'E'
           ELSE IF ( LPARM(J3)(10:20) .EQ. 'RIGHT ASCEN' ) THEN
!
! ----------- Source right ascension
!
              L_SPR = L_SPR + 1
              IND_SPR(L_SPR) = J3
              TYP_SPR(L_SPR) = RAS__SNX
              PAR_CODE(1:1) = 'C'
           ELSE IF ( LPARM(J3)(10:20) .EQ. 'DECLINATION' ) THEN
!
! ----------- Source declination
!
              L_SPR = L_SPR + 1
              IND_SPR(L_SPR) = J3
              TYP_SPR(L_SPR) = DCL__SNX
              PAR_CODE(1:1) = 'C'
           ELSE IF ( LPARM(J3)(10:20) .EQ. 'RIGHT ASC V' ) THEN
!
! ----------- Soure proper motion in right ascension
!
              L_SPR = L_SPR + 1
              IND_SPR(L_SPR) = J3
              TYP_SPR(L_SPR) = RAR__SNX
              PAR_CODE(1:1) = 'C'
           ELSE IF ( LPARM(J3)(10:20) .EQ. 'DEC VELO   ' ) THEN
!
! ----------- Source proper motion in declination
!
              L_SPR = L_SPR + 1
              IND_SPR(L_SPR) = J3
              TYP_SPR(L_SPR) = DCR__SNX
              PAR_CODE(1:1) = 'C'
           ELSE IF ( LPARM(J3)(10:20) .EQ. 'AXIS OFFSET' ) THEN
!
! ----------- Antenna axis offset
!
              L_SPR = L_SPR + 1
              IND_SPR(L_SPR) = J3
              TYP_SPR(L_SPR) = AXI__SNX
           ELSE
              CALL ERR_LOG ( 3720, IUER, 'WRITE_SINEX', 'Parameter '// &
     &             LPARM(J3)//' is not yet supported for putting into the '// &
     &            'SINEX output file. Please either upgrade Solve or exclude '// &
     &            'this parameter from thoese paramters which are put in the '// &
     &            'SINEX listing' )
              RETURN
         END IF
 430  CONTINUE
!
! --- Build the name of Solve revision date file
!
      FINAM = PRE_SOL_DIR(1:PRE_SOL_LEN)//REVISION_FILE
      INQUIRE ( FILE=FINAM, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 3721, IUER, 'WRITE_SINEX', 'The file with Solve '// &
     &         'revision date '//FINAM(1:I_LEN(FINAM))//' was not found' )
           RETURN
      END IF
!
! --- Open the filename with Solve revision date
!
      LUN = GET_UNIT() ! Get a free unit for I/O
      OPEN ( UNIT=LUN, FILE=FINAM, STATUS='OLD', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           WRITE ( 6, * ) 'IOS=',IOS
           CALL ERR_LOG ( 3722, IUER, 'WRITE_SINEX', 'Error in attempt '// &
     &         'to open file '//FINAM )
           RETURN
      END IF
!
! --- Read Solve revision file and get the latest revision date
!
      READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) REVISION_STR
      IF ( IOS .NE. 0 ) THEN
           WRITE ( 6, * ) 'IOS=',IOS
           CALL ERR_LOG ( 3723, IUER, 'WRITE_SINEX', 'Error in reading '// &
     &         'the file with revision date: '//FINAM )
           RETURN
      END IF
      CLOSE ( UNIT=LUN )
!
! --- Now open output Sinex file
!
      LOUT = GET_UNIT()
      OPEN  ( UNIT=LOUT, FILE=SINEX_REALNAME, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 3724, IUER, 'WRITE_SINEX', 'Error '// &
     &                    STR(1:I_LEN(STR))//' in an attempt to open output '// &
     &                   'Sinex listing file '//SINEX_REALNAME )
           RETURN
      END IF
!
! --- Get the current date/time in SINEX format
!
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_SINEX ( GET_CDATE(), DATE_NOW, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3725, IUER, 'WRITE_SINEX', 'Trap of internal '// &
     &         'control: error in transformation the date to SINEX format' )
           RETURN
      END IF
!
      IF ( FL_GLOBAL ) THEN
!
! -------- Get nominal start date of the first session in the list and
! -------- nominal end date of the last session in the list
!
           FJD_BEG = GLO_FJDOBS_MIN
           FJD_END = GLO_FJDOBS_MAX
         ELSE
!
! -------- Get the nominal dates of the first and the last observation in this
! -------- session
!
           FJD_BEG = JDATE_ALL_BEG
           FJD_END = JDATE_ALL_END
      END IF
!
! --- Transform the nominal start date to Sinex format
!
      CALL ERR_PASS ( IUER, IER )
      OUT(1:23) = JD_TO_DATE ( FJD_BEG, IER )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) 'Wrong FJD_BEG date: ',FJD_BEG
           CALL ERR_LOG ( 3726, IUER, 'WRITE_SINEX', 'Error in transorming '// &
     &         'nominal start date' )
           RETURN
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_SINEX ( OUT(1:23), DATE_BEG, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3727, IUER, 'WRITE_SINEX', 'Trap of internal '// &
     &         'control: error in transformation the date '//OUT(1:23)// &
     &         'to SINEX format' )
           RETURN
      END IF
!
! --- Transform the nominal end date to Sinex fomat
!
      CALL ERR_PASS ( IUER, IER )
      OUT(1:23) = JD_TO_DATE ( FJD_END, IER )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) 'Wrong FJD_END date: ',FJD_END
           CALL ERR_LOG ( 3728, IUER, 'WRITE_SINEX', 'Error in transorming '// &
     &         'nominal end date' )
           RETURN
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_SINEX ( OUT(1:23), DATE_END, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3729, IUER, 'WRITE_SINEX', 'Trap of internal '// &
     &         'control: error in transformation the date '//OUT(1:23)// &
     &         ' to SINEX format' )
           RETURN
      END IF
!
! --- Write down the number of parameters to be put in the Sinex listing
!
      CALL INCH   ( L_SPR, STR_PAR )
      CALL CHASHR (        STR_PAR )
      CALL BLANK_TO_ZERO ( STR_PAR )
!
! === Getnerate the header line ...
!
      OUT = '%=SNX '//SINEX_VERS(1:4)//' '//CENTER_ABR//' '// &
     &      DATE_NOW//' '//CENTER_ABR//' '// &
     &      DATE_BEG(1:12)//' '//DATE_END(1:12)//' R '// &
     &      STR_PAR//' 2 '//PAR_CODE
      WRITE ( LOUT , FMT='(A)' ) OUT(1:80)
!
! --- ... and write it down to the listing
!
!
! --- Get the tables of station names, station codes, descriptions, etc.
!
      CALL ERR_PASS ( IUER, IER )
      CALL GET_NS_TABLE ( MASTER_DIR, M_STA, L_STA, STA_NAME, STA_CODE, &
     &                    STA_DOME, STA_CDP, STA_DESC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3730, IUER, 'WRITE_SINEX', 'Error in attempt to '// &
     &         'read the table with station names, codes, descriptions, etc.' )
           RETURN
      END IF
!
! --- Get system information: user name, user real name, email address,
! --- system name, node name and hardware name
!
      CALL GETINFO_USER   ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      CALL GETINFO_SYSTEM ( SYSNAME, NODENAME, HARDWARE )
!
! --- Write extended comment
!
      WRITE ( LOUT, '(A)' ) '*'
      WRITE ( LOUT, '(A)' ) '* Created on:   '//GET_CDATE()//' local time'
      WRITE ( LOUT, '(A)' ) '* Created at:   '//CENTER_FULL_NAME
      WRITE ( LOUT, '(A)' ) '* Created by:   '// &
     &                       USER_REALNAME(1:I_LEN(USER_REALNAME))//' ( '// &
     &                       USER_E_ADDRESS(1:I_LEN(USER_E_ADDRESS))//' )'
      WRITE ( LOUT, '(A)' ) '* Generated by: routine '//ROUTINE__LABEL
      WRITE ( LOUT, '(A)' ) '*'
      WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                      '---------------------------------------'
      WRITE ( LOUT, '(A)' ) '*'
!
! === Write down FILE/REFERENCE section
!
      WRITE ( LOUT, '(A)' ) '+FILE/REFERENCE'
      WRITE ( LOUT, '(A)' ) ' DESCRIPTION       '//CENTER_ABR
      IF ( FL_GLOBAL ) THEN
           WRITE ( LOUT, '(A)' ) ' OUTPUT            Global VLBI solution'
         ELSE
           WRITE ( LOUT, '(A)' ) ' OUTPUT            Single session VLBI '// &
     &            'solution'
      END IF
      WRITE ( LOUT, '(A)' ) ' CONTACT           '// &
     &        USER_E_ADDRESS(1:I_LEN(USER_E_ADDRESS))//' <'// &
     &        USER_REALNAME(1:I_LEN(USER_REALNAME))//'>'
      WRITE ( LOUT, '(A)' ) ' SOFTWARE          VLBI analysis system '// &
     &                      'Calc/Solve, revision date '// &
     &                       REVISION_STR(1:I_LEN(REVISION_STR))
      WRITE ( LOUT, '(A)' ) ' HARDWARE          HP'// &
     &                        HARDWARE(1:I_LEN(HARDWARE))//' '// &
     &                        SYSNAME(1:I_LEN(SYSNAME))
      IF ( FL_GLOBAL ) THEN
           WRITE ( LOUT, '(A,I5,A)' ) ' INPUT             A set of ', NARCS, &
     &                                ' VLBI experiments'
         ELSE
           WRITE ( LOUT, '(A)' ) ' INPUT             VLBI experiment '// &
     &             SESS_CODE(1:I_LEN(SESS_CODE))//', database '//DB_NAME// &
     &            ' version '//VERS_STR(1:I_LEN(VERS_STR))
      END IF
      WRITE ( LOUT, '(A)' ) '-FILE/REFERENCE'
      WRITE ( LOUT, '(A)' ) '*'
      WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                      '---------------------------------------'
      WRITE ( LOUT, '(A)' ) '*'
!
! === FILE/COMMENT block
!
      WRITE ( LOUT, '(A)' ) '+FILE/COMMENT'
!
! --- Now time to put comments into the file
!
      FINAM = SINEX_COMFIL
!
! --- Open comments file
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FINAM, STATUS='OLD', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           WRITE ( 6, * ) 'IOS=',IOS
           CALL ERR_LOG ( 3731, IUER, 'WRITE_SINEX', 'Error in attempt '// &
     &         'to open the input comments file '//FINAM )
           RETURN
      END IF
!
! --- Copying contents of the comment file to the listing
!
      DO 440 J4=1,1024*1024
         READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR
         IF ( IOS .EQ. -1 ) THEN
              GOTO 840
          ELSE IF ( IOS .NE. 0 ) THEN
              WRITE ( 6, * ) 'IOS=',IOS
              CALL ERR_LOG ( 3732, IUER, 'WRITE_SINEX', 'Error in reading '// &
     &            'the file with sinex comments: '//FINAM )
              RETURN
         END IF
         WRITE ( UNIT=LOUT, FMT='(A)' ) ' '//STR(1:I_LEN(STR))
 440  CONTINUE
 840  CONTINUE
      CLOSE ( UNIT=LUN )
!
! --- Now try to put in the sinex listing contents of the file with format
! --- description, since some folks cleaimed that they do not understand
! --- what is in the sinex listing
!
      FINAM = SOLVE_HELP_DIR//SINEX_FORMAT_DESCR_FILE
      INQUIRE ( FILE=FINAM, EXIST=LEX )
      IF ( LEX ) THEN
           WRITE ( UNIT=LOUT, FMT='(A)' ) ' '
           WRITE ( UNIT=LOUT, FMT='(A)' ) '>>> FORMAT DESCRIPTION <<< '
           WRITE ( UNIT=LOUT, FMT='(A)' ) '>>> ################## <<< '
           WRITE ( UNIT=LOUT, FMT='(A)' ) ' '
!
           OPEN ( UNIT=LUN, FILE=FINAM, STATUS='OLD', IOSTAT=IOS )
           IF ( IOS .NE. 0 ) THEN
                WRITE ( 6, * ) 'IOS=',IOS
                CALL ERR_LOG ( 3733, IUER, 'WRITE_SINEX', 'Error in attempt '// &
     &              'to open the input comments file '//FINAM )
                RETURN
           END IF
!
! -------- Copying contents of the comment file to the listing
!
           DO 450 J5=1,1024*1024
              READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR
              IF ( IOS .EQ. -1 ) THEN
                   GOTO 850
                 ELSE IF ( IOS .NE. 0 ) THEN
                   WRITE ( 6, * ) 'IOS=',IOS
                   CALL ERR_LOG ( 3734, IUER, 'WRITE_SINEX', 'Error in '// &
     &                 'reading the file with sinex comments: '//FINAM )
                   RETURN
              END IF
              WRITE ( UNIT=LOUT, FMT='(A)' ) STR(1:I_LEN(STR))
 450       CONTINUE
 850       CONTINUE
           CLOSE ( UNIT=LUN )
      END IF
!
! --- Build the name of CNTR-scratch file.
!
      FINAM = PRE_SCR_DIR(1:PRE_SD_LEN)//'CNTR'//PRE_LETRS
      INQUIRE ( FILE=FINAM, EXIST=LEX )
      IF ( LEX ) THEN
!
! -------- Well. This file exists. Very well. The file consists of one line.
! -------- The line has two words separated by blanks: the first word 
! -------- is the machine name and the second word is the control file name
!
! -------- Allocate memory for the buffer
!
           MBUF = 8192
           ALLOCATE ( BUF(MBUF), STAT=IOS )
           IF ( IOS .NE. 0 ) THEN
                CALL CLRCH ( STR ) 
                CALL IINCH ( MBUF*256, STR )
                CALL ERR_LOG ( 3735, IUER, 'WRITE_SINEX', 'Error in an '// &
     &              'attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &              'of dynamic memory' )
                RETURN 
           END IF
!
! -------- Read CNTR-scratch file
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( FINAM, MBUF, BUF, NBUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3736, IUER, 'WRITE_SINEX', 'Error in an '// &
     &              'attempt to read scratch file '//FINAM )
                DEALLOCATE ( BUF )
                RETURN 
           END IF
!
! -------- Split the line into words
!
           CALL EXWORD   ( BUF(1), MIND, LIND, IND, REG, 0 )
           IF ( LIND .GE. 2 ) THEN
!
! ------------- Extract the name of the control file
!
                FINAM = BUF(1)(IND(1,2):IND(2,2) )
                INQUIRE ( FILE=FINAM, EXIST=LEX )
                IF ( LEX ) THEN
!
! ------------------ Read the control files
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL RD_TEXT  ( FINAM, MBUF, BUF, NBUF, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 3737, IUER, 'WRITE_SINEX', 'Error '// &
     &                        'in an attempt to read file '//FINAM )
                          DEALLOCATE ( BUF )
                          RETURN 
                     END IF
!
                     CALL FILE_INFO ( FINAM(1:I_LEN(FINAM))//CHAR(0), &
     &                                UNIX_DATE, SIZE_I8 )
                     STR(1:19) = UNIX_DATE_TO_DATE ( UNIX_DATE )
!
                     WRITE ( UNIT=LOUT, FMT='(A)' ) ' '
                     WRITE ( UNIT=LOUT, FMT='(A)' ) '>>> Solution Control file <<< '
                     WRITE ( UNIT=LOUT, FMT='(A)' ) '>>> ##################### <<< '
                     WRITE ( UNIT=LOUT, FMT='(A)' ) ' '
                     WRITE ( UNIT=LOUT, FMT='(A)' ) 'Refer to http://gemini.gsfc.nasa.gov/solve_root/help/solve_guide_03.html'
                     WRITE ( UNIT=LOUT, FMT='(A)' ) 'for description the syntax of Mark5 VLBI analysis software control files'
                     WRITE ( UNIT=LOUT, FMT='(A)' ) ' '
                     WRITE ( UNIT=LOUT, FMT='(A)' ) 'Local file name: '//FINAM(1:I_LEN(FINAM))
                     WRITE ( UNIT=LOUT, FMT='(A,I6)' ) 'File length: ', SIZE_I8
                     WRITE ( UNIT=LOUT, FMT='(A)' ) 'Modification date: '//STR(1:19)
                     WRITE ( UNIT=LOUT, FMT='(A)' ) ' '
!
! ------------------ Write control file line by line into the listing
!
                     DO 460 J6=1,NBUF
                        WRITE ( UNIT=LOUT, FMT='(A)' ) BUF(J6)(1:I_LEN(BUF(J6)))
 460                 CONTINUE 
                     WRITE ( UNIT=LOUT, FMT='(A)' ) ' '
                END IF
           END IF
           DEALLOCATE ( BUF )
      END IF
!
      WRITE ( LOUT, '(A)' ) '-FILE/COMMENT'
      WRITE ( LOUT, '(A)' ) '*'
      WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                      '---------------------------------------'
      WRITE ( LOUT, '(A)' ) '*'
!
! --- Now time to put acknowledgments into the file
!
      FINAM = SINEX_ACKFIL
!
! === INPUT/ACKNOWLEDGEMENTS section
!
      WRITE ( LOUT, '(A)' ) '+INPUT/ACKNOWLEDGEMENTS'
!
! --- Open acknowledgment file
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FINAM, STATUS='OLD', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 3738, IUER, 'WRITE_SINEX', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open acknowledgement '// &
     &         'file '//FINAM )
           RETURN
      END IF
!
! --- Copy contents of acknowledgement file into Sinex listing
!
      DO 470 J7=1,1024*1024
         READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR
         IF ( IOS .EQ. -1 ) THEN
              GOTO 870
           ELSE IF ( IOS .NE. 0 ) THEN
              WRITE ( 6, * ) 'IOS=',IOS
              CALL ERR_LOG ( 3739, IUER, 'WRITE_SINEX', 'Error in reading '// &
     &            'the file with sinex acknowledgments: '//FINAM )
              RETURN
         END IF
         WRITE ( UNIT=LOUT, FMT='(A)' ) STR(1:I_LEN(STR))
 470  CONTINUE
 870  CONTINUE
      CLOSE ( UNIT=LUN )
      WRITE ( LOUT, '(A)' ) '-INPUT/ACKNOWLEDGEMENTS'
      WRITE ( LOUT, '(A)' ) '*'
      WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                      '---------------------------------------'
      WRITE ( LOUT, '(A)' ) '*'
!
! === SITE/ID section
!
      WRITE ( LOUT, '(A)' ) '+SITE/ID'
      WRITE ( LOUT, '(A)' ) '*Code PT Domes____ T Station description___ '// &
     &                      'Approx_lon_ Approx_lat_ App_h__'
!
      CALL NOUT_I4 ( M_STA, STA_PARIND )
      CALL NOUT    ( M_STA*LEN(PT_DAT(1)), PT_DAT )
!
      ICDP_REPL = 9001
      DO 480 J8=1,NUMSTA
!
! ------ Bypass the station which was deselected from solution
!
         IF ( .NOT. CHECK_STABIT( INT2(J8) ) ) GOTO 480
         IF ( FL_GLOBAL  .AND.  VSITED(J8) .NE. 0.0D0 ) THEN
              STA_PARIND(J8) = STA_PARIND(J8) + 1
              CALL MDYJL ( IM_I2, ID_I2, IY_I2, IT_I2, VSITED(J8) )
              WRITE ( UNIT=PT_DAT(J8), FMT='(3I2)' ) IY_I2, IM_I2, ID_I2
              CALL BLANK_TO_ZERO ( PT_DAT(J8) )
         END IF
!
         STA_NAM_CLEAN = ISITN_CHR(J8) 
         CALL VTD_NAME_REPAIR ( STA_NAM_CLEAN )
         ISTA = LTM_DIF ( 1, L_STA, STA_NAME, STA_NAM_CLEAN )
         IF ( ISTA .LE. 0 ) THEN
              CALL ERR_LOG ( 3740, IUER, 'WRITE_SINEX', 'Trap of internal '// &
     &            'control: station '//STA_NAM_CLEAN//' was not found in '// &
     &            'the station list of valid IVS networking stations '// &
     &            'defined in the file '// &
     &            MASTER_DIR(1:I_LEN(MASTER_DIR))//NAMSTA_FILE )
              RETURN
         END IF
!
! ------ Get approximate latitude, longitude and the height above reference
! ------ ellipsoid fo this station
!
         CALL PLH ( VSITEC(1,J8), LATITUDE, LONGITUDE, HEIGHT )
         IF ( LONGITUDE .GT.  PI2 ) LONGITUDE = LONGITUDE - PI2
         IF ( LONGITUDE .LT.  0.0 ) LONGITUDE = LONGITUDE + PI2
!
! ------ Transform then in grad/minutes
!
         CALL RG_TAT ( LATITUDE,  1, LATT_STR, 0 )
         CALL RG_TAT ( LONGITUDE, 1, LONG_STR, 0 )
!
         IF ( STA_CDP(ISTA) .LE. 0  .OR.  STA_CDP(ISTA) .GT. 9999 ) THEN
!
! ----------- Temporary (of 2002.10.01) measure for setting a bogus CDP
! ----------- dome number for the stations which do not have it.
!
              STA_CDP(ISTA) = ICDP_REPL
              ICDP_REPL = ICDP_REPL + 1
         END IF
!
         WRITE ( LOUT, 110 ) STA_CDP(ISTA), STA_DOME(ISTA), STA_NAME(ISTA), &
     &                       STA_DESC(ISTA)(1:13), &
     &              LONG_STR(1:3)//' '//LONG_STR(5:6)//' '//LONG_STR(8:11), &
     &              LATT_STR(1:3)//' '//LATT_STR(5:6)//' '//LATT_STR(8:11), &
     &              HEIGHT
 110     FORMAT ( 1X, I4, 1X,' A', 1X,A, ' R ', A, 1X, A, 1X, A11, 1X, A11, &
     &            1X, F7.1 )
 480  CONTINUE
      WRITE ( LOUT, '(A)' ) '-SITE/ID'
!
      WRITE ( LOUT, '(A)' ) '*'
      WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                      '---------------------------------------'
      WRITE ( LOUT, '(A)' ) '*'
!
! === SOURCE/ID section
!
      WRITE ( LOUT, '(A)' ) '+SOURCE/ID'
      WRITE ( LOUT, '(A)' ) '*Code IERS nam ICRF designator  IAU name   '// &
     &                      'IVS name'
!
! --- Get the table of correpondence between IVS source name, IERS source
! --- name, ICRF name, J2000 IAU name
!
      CALL CLRCH ( SAVE_DIR )
      SAVE_DIR = PRE_SAV_DIR
      CALL ERR_PASS ( IUER, IER )
      CALL GET_SOURCE_TABLE ( SAVE_DIR, M_SRC, L_SRC, IVS_NAME, IERS_NAME, &
     &                        ICRF_NAME, J2000_NAME, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3741, IUER, 'WRITE_SINEX', 'Error in reading '// &
     &         'the file with source names' )
           RETURN
      END IF
!
      DO 490 J9=1,NUMSTR
         CALL INCH ( J9,  SOURCE_ID(J9) )
         CALL CHASHR    ( SOURCE_ID(J9) )
         CALL BLANK_TO_ZERO ( SOURCE_ID(J9) )
!
! ------ Search the source name in the IVS_NAME table
!
         IP = LTM_DIF ( 1, L_SRC, IVS_NAME, ISTRN_CHR(J9) )
         IF ( IP .LE. 0 ) THEN
               WRITE ( 6, * ) ' M_SRC=',M_SRC, ' L_SRC =',L_SRC
              CALL ERR_LOG ( 3742, IUER, 'WRITE_SINEX', 'Trap of internal '// &
     &            'control: soruce '//ISTRN_CHR(J9)//' was not found in '// &
     &            'source list file of valid source names '// &
     &            SAVE_DIR(1:I_LEN(SAVE_DIR))//NAMSRC_FILE )
              RETURN
         END IF
!
! ------ Transform the start date of the J9-th source to SINEX format
!
         IF ( .NOT. FL_GLOBAL ) THEN
              SRC_FJD_BEG(J9) = JDATE_ALL_BEG
         END IF
         CALL ERR_PASS ( IUER, IER )
         DATE_BEG = JD_TO_DATE ( SRC_FJD_BEG(J9), IER )
!
         IF ( IER .NE. 0 ) THEN
              WRITE ( 6, * ) ' J9=',' SRC_FJD_BEG(J9) =', SRC_FJD_BEG(J9)
              CALL ERR_LOG ( 3743, IUER, 'WRITE_SINEX', 'Error in '// &
     &            'an attempt to transform the start date of source '// &
     &             ISTRN_CHR(J9) )
              RETURN
         END IF
!
         CALL DATE_TO_SINEX ( DATE_BEG, SRC_DATE_BEG(J9), IER )
!
! ------ Transform the end date of the J9-th source to SINEX format
!
         IF ( .NOT. FL_GLOBAL ) THEN
              SRC_FJD_END(J9) = JDATE_ALL_END
         END IF
         CALL ERR_PASS ( IUER, IER )
         DATE_END = JD_TO_DATE ( SRC_FJD_END(J9), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3744, IUER, 'WRITE_SINEX', 'Error in '// &
     &            'an attempt to transform the end date of '// &
     &            'source '//ISTRN_CHR(J9) )
              RETURN
         END IF
         CALL DATE_TO_SINEX ( DATE_END, SRC_DATE_END(J9), IER )
         OUT(30:41) = SRC_DATE_END(J9)
!
! ------ Transform the middle date of the J9-th source to SINEX format
!
         IF ( .NOT. FL_GLOBAL ) THEN
              SRC_FJD_MID(J9) = (JDATE_ALL_BEG + JDATE_ALL_END)/2.0D0
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         DATE_MID = JD_TO_DATE ( SRC_FJD_MID(J9), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3745, IUER, 'WRITE_SINEX', 'Error in '// &
     &            'an attempt to transform the middle date of '// &
     &            'source '//ISTRN_CHR(J9) )
              RETURN
         END IF
         CALL DATE_TO_SINEX ( DATE_MID, SRC_DATE_MID(J9), IER )
!
         WRITE ( LOUT, 170 ) SOURCE_ID(J9), IERS_NAME(IP), ICRF_NAME(IP), &
     &                       J2000_NAME(IP), IVS_NAME(IP)
 170     FORMAT ( 1X, A4, 1X,A, 1X,A, 1X,A, 1X,A )
 490  CONTINUE
      WRITE ( LOUT, '(A)' ) '-SOURCE/ID'
!
      WRITE ( LOUT, '(A)' ) '*'
      WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                      '---------------------------------------'
      WRITE ( LOUT, '(A)' ) '*'
!
! === Faked SITE/RECEIVER section
!
      WRITE ( LOUT, '(A)' ) '+SITE/RECEIVER'
      WRITE ( LOUT, '(A)' ) '*Code PT SBIN T Data_Start__ Data_End____ '// &
     &                      'Receiver Type_______ S/N__ Firmware_ID'
      DO 4100 J10=1,NUMSTA ! Cycle over stations
         IF ( .NOT. CHECK_STABIT( INT2(J10) ) ) GOTO 4100
         CALL CLRCH ( OUT )
         STA_NAM_CLEAN = ISITN_CHR(J10) 
         CALL VTD_NAME_REPAIR ( STA_NAM_CLEAN )
         ISTA = LTM_DIF ( 1, L_STA, STA_NAME, STA_NAM_CLEAN )
         CALL INCH  ( STA_CDP(ISTA), OUT(2:5) )
         OUT(7:8)   = ' A'
         CALL INCH ( STA_PARIND(J10)+1, OUT(10:13) )
         CALL CHASHR ( OUT(10:13) )
         OUT(15:15) = 'R'
!
! ------ Transform the start date of the J10-th station to SINEX format
!
         CALL ERR_PASS ( IUER, IER )
         IF ( FL_GLOBAL ) THEN
              DATE_BEG = JD_TO_DATE ( STA_FJD_BEG(J10), IER )
            ELSE
              DATE_BEG = JD_TO_DATE ( JDATE_STA_BEG(J10), IER )
         END IF
!
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3746, IUER, 'WRITE_SINEX', 'Error in '// &
     &            'an attempt to transform the start date of station '// &
     &             ISITN_CHR(J10) )
              RETURN
         END IF
!
         CALL DATE_TO_SINEX ( DATE_BEG, STA_DATE_BEG(J10), IER )
         OUT(17:28) = STA_DATE_BEG(J10)
!
! ------ Transform the end date of the J10-th station to SINEX format
!
         CALL ERR_PASS ( IUER, IER )
         IF ( FL_GLOBAL ) THEN
              DATE_END = JD_TO_DATE ( STA_FJD_END(J10), IER )
            ELSE
              DATE_END = JD_TO_DATE ( JDATE_STA_END(J10), IER )
         END IF
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3747, IUER, 'WRITE_SINEX', 'Error in '// &
     &            'an attempt to transform the end date of '// &
     &            'station '//ISITN_CHR(J10) )
              RETURN
         END IF
         CALL DATE_TO_SINEX ( DATE_END, STA_DATE_END(J10), IER )
         OUT(30:41) = STA_DATE_END(J10)
!
! ------ Transform the middle date of the J10-th station to SINEX format
!
         CALL ERR_PASS ( IUER, IER )
         IF ( FL_GLOBAL ) THEN
              DATE_MID = JD_TO_DATE ( STA_FJD_MID(J10), IER )
            ELSE
              DATE_MID = JD_TO_DATE ( JDATE_STA_MID(J10), IER )
         END IF
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3748, IUER, 'WRITE_SINEX', 'Error in '// &
     &            'an attempt to transform the middle date of '// &
     &            'station '//ISITN_CHR(J10) )
              RETURN
         END IF
         CALL DATE_TO_SINEX ( DATE_MID, STA_DATE_MID(J10), IER )
!
         OUT(43:80) = '----VLBI Station---- --NM- -----NA----'
         IF ( STA_PARIND(J10) .EQ. 0 ) THEN ! bypass the second station parameter
              WRITE ( UNIT=LOUT, FMT='(A)' ) OUT(1:I_LEN(OUT))
!@              WRITE ( UNIT=LOUT, FMT='(A)' ) OUT(1:80)
         END IF
 4100 CONTINUE
!
      WRITE ( LOUT, '(A)' ) '-SITE/RECEIVER'
      WRITE ( LOUT, '(A)' ) '*'
      WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                      '---------------------------------------'
      WRITE ( LOUT, '(A)' ) '*'
!
! === SITE/ANTENNA section. The same as previous
!
      WRITE ( LOUT, '(A)' ) '+SITE/ANTENNA'
      WRITE ( LOUT, '(A)' ) '*Code PT SBIN T Data_Start__ Data_End____ '// &
     &                      'Receiver Type_______ S/N__'
!
      DO 4110 J11=1,NUMSTA
         IF ( .NOT. CHECK_STABIT( INT2(J11) ) ) GOTO 4110
         CALL CLRCH ( OUT )
         STA_NAM_CLEAN = ISITN_CHR(J11) 
         CALL VTD_NAME_REPAIR ( STA_NAM_CLEAN )
         ISTA = LTM_DIF ( 1, L_STA, STA_NAME, STA_NAM_CLEAN )
         CALL INCH  ( STA_CDP(ISTA), OUT(2:5) )
         OUT(7:8)   = ' A'
         CALL INCH ( STA_PARIND(J11)+1, OUT(10:13) )
         CALL CHASHR ( OUT(10:13) )
         OUT(15:15) = 'R'
         OUT(17:28) = STA_DATE_BEG(J11)
         OUT(30:41) = STA_DATE_END(J11)
         OUT(43:68) = '----VLBI Station---- --NM-'
         IF ( STA_PARIND(J11) .EQ. 0 ) THEN ! bypass the second station parameter
              WRITE ( UNIT=LOUT, FMT='(A)' ) OUT(1:I_LEN(OUT))
!@              WRITE ( UNIT=LOUT, FMT='(A)' ) OUT(1:80)
         END IF
 4110 CONTINUE
!
      WRITE ( LOUT, '(A)' ) '-SITE/ANTENNA'
      WRITE ( LOUT, '(A)' ) '*'
      WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                      '---------------------------------------'
      WRITE ( LOUT, '(A)' ) '*'
!
! === SITE/ECCENTRICITY section
!
      WRITE ( LOUT, '(A)' ) '+SITE/ECCENTRICITY'
      WRITE ( LOUT, '(A)' ) '*Code PT SBIN T Data_Start__ Data_End____ typ '// &
     &                      'Apr --> Benchmark (m)_______'
!
      IF ( FL_GLOBAL ) THEN
!
! -------- Determine eccentricity file
!
           CALL CLRCH  ( STR         )
           CALL CLRCH  ( ECCDAT_FILE )
           CALL GETENVAR ( 'ECC_DATA_FILE', STR )
           IF ( ILEN(STR) .GT. 0 ) THEN
                ECCDAT_FILE = STR
              ELSE
!
! ------------- Get default file
!
                ECCDAT_FILE = ECC_DATA_FILE
           END IF
           IF ( ECCDAT_FILE(1:1) .NE. '/' ) THEN
                ECCDAT_FILE = PRE_SAV_DIR(1:PRE_SV_LEN)//ECCDAT_FILE
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( ECCDAT_FILE, M_ECC, BUF_ECC, N_ECC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3749, IUER, 'WRITE_SINEX', 'Error in '// &
     &              'reading file with antenna eccentricities '//ECCDAT_FILE )
                RETURN
           END IF
!
           IF ( BUF_ECC(1)(1:LEN(ECC__LABEL)) .NE. ECC__LABEL ) THEN
                CALL ERR_LOG ( 3750, IUER, 'WRITE_SINEX', 'Trap of internal '// &
     &              'control: file '//ECCDAT_FILE(1:I_LEN(ECCDAT_FILE))// &
     &              'is not an eccentricity file, since its frist line '// &
     &              ' is '//BUF_ECC(1)(1:I_LEN(BUF_ECC(1)))// &
     &              ' but not expected '//ECC__LABEL )
                RETURN
           END IF
      END IF
!
      DO 4120 J12=1,NUMSTA
         IF ( .NOT. CHECK_STABIT( INT2(J12) ) ) GOTO 4120
!
         CALL CLRCH ( OUT )
         STA_NAM_CLEAN = ISITN_CHR(J12) 
         CALL VTD_NAME_REPAIR ( STA_NAM_CLEAN )
         ISTA = LTM_DIF ( 1, L_STA, STA_NAME, STA_NAM_CLEAN )
         CALL INCH  ( STA_CDP(ISTA), OUT(2:5) )
         CALL INCH  ( STA_CDP(ISTA), OUT(2:5) )
         OUT(7:8)   = ' A'
         CALL INCH ( STA_PARIND(J12)+1, OUT(10:13) )
         CALL CHASHR ( OUT(10:13) )
         OUT(15:15) = 'R'
!
! ------ Write down eccentricity vectors
!
         IF ( FL_GLOBAL ) THEN
!
! ----------- In the case of global mode we cscan eccentricity file for
! ----------- ALL the records related to the J12 station
!
              DO 4130 J13=1,N_ECC
                 IF ( BUF_ECC(J13)(1:1)  .NE. ' ' ) GOTO 4130
                 IF ( BUF_ECC(J13)(3:10) .EQ. ISITN_CHR(J12) ) THEN
!
! ------------------- Prepare the date ofthe eccentricity. If the eccentricity
! ------------------- file has datae 1970.01.01 or 2050.01.01 they are replaced
! ------------------- with 00:000:00000 according to Sinex specifications
!
                      CALL DATE_TO_TIME ( BUF_ECC(J13)(18:33)//':00.0', &
     &                                    MJD, SEC, 0 )
                      FJD_ECC = J2000__JD + (MJD - J2000__MJD) + SEC/86400.0
                      IF ( FJD_ECC .LT. FJD_ECC_MIN ) THEN
                           OUT(17:28) = '00:000:00000'
                         ELSE
                           CALL DATE_TO_SINEX ( BUF_ECC(J13)(18:33)//':00.0', &
     &                                          OUT(17:28), IER )
                      END IF
!
                      CALL DATE_TO_TIME ( BUF_ECC(J13)(36:51)//':00.0', &
     &                                    MJD, SEC, 0 )
                      FJD_ECC = J2000__JD + (MJD - J2000__MJD) + SEC/86400.0
                      IF ( FJD_ECC .GT. FJD_ECC_MAX ) THEN
                           OUT(30:41) = '00:000:00000'
                         ELSE
                           CALL DATE_TO_SINEX ( BUF_ECC(J13)(36:51)//':00.0', &
     &                                          OUT(30:41), IER )
                      END IF
!
                      IF ( BUF_ECC(J13)(88:90) .EQ. 'XYZ' ) THEN
                           OUT(43:45) = 'XYZ'
                           OUT(46:54) = ' '//BUF_ECC(J13)(56:63)
                           OUT(55:63) = ' '//BUF_ECC(J13)(67:74)
                           OUT(64:72) = ' '//BUF_ECC(J13)(78:85)
                        ELSE
                           OUT(43:45) = 'UNE'
                           OUT(46:54) = ' '//BUF_ECC(J13)(78:85)
                           OUT(55:63) = ' '//BUF_ECC(J13)(56:63)
                           OUT(64:72) = ' '//BUF_ECC(J13)(67:74)
                      END IF
!
                      CALL BLANK_TO_ZERO ( OUT(49:54) )
                      CALL BLANK_TO_ZERO ( OUT(58:63) )
                      CALL BLANK_TO_ZERO ( OUT(67:72) )
!
                      IF ( STA_PARIND(J12) .EQ. 0 ) THEN ! bypass the second station parameter
                           WRITE ( UNIT=LOUT, FMT='(A)' ) OUT(1:I_LEN(OUT))
!@                           WRITE ( UNIT=LOUT, FMT='(A)' ) OUT(1:80)
                      END IF
                 END IF
 4130         CONTINUE
           ELSE
!
! ----------- In the case of local solution this information is kept in
! ----------- ECC_NEW in flyby.i file
!
              OUT(17:28) = STA_DATE_BEG(J12)
              OUT(30:41) = STA_DATE_END(J12)
              OUT(43:45) = 'XYZ'
              WRITE ( UNIT=OUT(46:72), FMT='(3(1X,F8.4))' ) ECC_NEW(1,J12), &
     &                ECC_NEW(2,J12),  ECC_NEW(3,J12)
              IF ( STA_PARIND(J12) .EQ. 0 ) THEN ! bypass the second station parameter
                   WRITE ( UNIT=LOUT, FMT='(A)' ) OUT(1:I_LEN(OUT))
!@                   WRITE ( UNIT=LOUT, FMT='(A)' ) OUT(1:80)
              END IF
         END IF
 4120 CONTINUE
!
      WRITE ( LOUT, '(A)' ) '-SITE/ECCENTRICITY'
      WRITE ( LOUT, '(A)' ) '*'
      WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                      '---------------------------------------'
      WRITE ( LOUT, '(A)' ) '*'
!
! === Site/epoch section
!
      WRITE ( LOUT, '(A)' ) '+SOLUTION/EPOCHS'
      WRITE ( LOUT, '(A)' ) '*Code PT SBIN T Data_start__ Data_end____ '// &
     &                      'Mean_epoch__'
      DO 4140 J14=1,NUMSTA
         IF ( .NOT. CHECK_STABIT( INT2(J14) ) ) GOTO 4140
!
         CALL CLRCH ( OUT )
         STA_NAM_CLEAN = ISITN_CHR(J14) 
         CALL VTD_NAME_REPAIR ( STA_NAM_CLEAN )
         ISTA = LTM_DIF ( 1, L_STA, STA_NAME, STA_NAM_CLEAN )
         CALL INCH  ( STA_CDP(ISTA), OUT(2:5) )
         OUT(7:8)   = ' A'
         CALL INCH ( STA_PARIND(J14)+1, OUT(10:13) )
         CALL CHASHR ( OUT(10:13) )
         OUT(15:15) = 'R'
         OUT(17:28) = STA_DATE_BEG(J14)
         OUT(30:41) = STA_DATE_END(J14)
         OUT(43:54) = STA_DATE_MID(J14)
!
         WRITE ( UNIT=LOUT, FMT='(A)' ) OUT(1:I_LEN(OUT))
!@         WRITE ( UNIT=LOUT, FMT='(A)' ) OUT(1:80)
 4140 CONTINUE
!
      WRITE ( LOUT, '(A)' ) '-SOLUTION/EPOCHS'
      WRITE ( LOUT, '(A)' ) '*'
      WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                      '---------------------------------------'
!
! === NUTATION/DATA section
!
      IF ( CALCV > -100.0D0  .AND. &
     &     ( CALCV < 9.0D0-SMALL_EPS  .OR. CALCV > 10.0D0+SMALL_EPS ) .AND. &
     &     .NOT. ( CALCV > 99.9D0-SMALL_EPS .AND. &
     &             CALCV < 99.9D0+SMALL_EPS       ) ) THEN
           WRITE ( UNIT=STR, FMT='(A,F8.3)' ) 'Calc_version: ',CALCV
           CALL ERR_LOG ( 3751, IUER, 'WRITE_SINEX', 'Unknown Calc version: '// &
     &          STR(1:I_LEN(STR))//' Supported versions: 9.0, 9.1, '// &
     &         '9.11, 9.12, 9.13, 10.0' )
           RETURN
      END IF
!
      WRITE ( LOUT, '(A)' ) '*'
      WRITE ( LOUT, '(A)' ) '+NUTATION/DATA'
      IF ( CALCV > 9.0D0-SMALL_EPS .AND. CALCV < 9.13D0+SMALL_EPS ) THEN
           WRITE ( LOUT, '(A)' ) ' IERS1996 APR IERS1996 apriori nutation '// &
     &                           'expansion was used'
           WRITE ( LOUT, '(A)' ) ' NONE     REF Total nutation angles are '// &
     &                           'reported in estimation block'
         ELSE IF ( CALCV > 10.0D0-SMALL_EPS .AND. CALCV < 10.0D0+SMALL_EPS ) THEN
           WRITE ( LOUT, '(A)' ) ' MHB2000  APR MHB2000  apriori nutation '// &
     &                           'expansion was used'
           WRITE ( LOUT, '(A)' ) ' NONE     REF Total nutation angles are '// &
     &                           'reported in estimation block'
      END IF
      WRITE ( LOUT, '(A)' ) '-NUTATION/DATA'
      WRITE ( LOUT, '(A)' ) '*'
      WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                      '---------------------------------------'
!
! === PRECESSION/DATA section
!
      WRITE ( LOUT, '(A)' ) '*'
      WRITE ( LOUT, '(A)' ) '+PRECESSION/DATA'
      IF ( CALCV > 9.0D0-SMALL_EPS .AND. CALCV < 9.13D0+SMALL_EPS ) THEN
           WRITE ( LOUT, '(A)' ) ' IERS1996 apriori precession constant and '// &
     &                      'obliquity rates were used '
         ELSE IF ( CALCV > 10.0D0-SMALL_EPS .AND. CALCV < 10.0D0+SMALL_EPS ) THEN
           WRITE ( LOUT, '(A)' ) ' CAPITAINE2003 apriori precession constant '// &
     &                      'and obliquity rates were used '
      END IF
      WRITE ( LOUT, '(A)' ) '-PRECESSION/DATA'
      WRITE ( LOUT, '(A)' ) '*'
      WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                      '---------------------------------------'
      WRITE ( LOUT, '(A)' ) '*'
      IF ( FL_GLOBAL ) THEN
!
! -------- This section is not written in global mode here, since statistics
! -------- will be computed at the very and of BACK run. THen HAUSR will append
! -------- this block
!
         ELSE
!
! ======== SOLUTION/STATISTICS block
!
           WRITE ( UNIT=LOUT, FMT='(A)' ) '+SOLUTION/STATISTICS'
           WRITE ( LOUT, '(A)' )          '* Units for WRMS: sec'
           WRITE ( UNIT=LOUT, FMT=180 ) 'NUMBER OF OBSERVATIONS        ', &
     &                                   STAT_NUMOBS
 180       FORMAT ( 1X,A30, 1X,I12 )
           WRITE ( UNIT=LOUT, FMT=180 ) 'NUMBER OF UNKNOWNS            ', &
     &                                   STAT_NUMUNK
           WRITE ( UNIT=LOUT, FMT=190 ) 'WEIGHTED SQUARE SUM OF O-C    ', &
     &                                   STAT_SQUOC
           WRITE ( UNIT=LOUT, FMT=190 ) 'SQUARE SUM OF RESIDUALS (VTPV)', &
     &                                   STAT_SQURES
 190       FORMAT ( 1X,A30, 1X,1PD21.14 )
           WRITE ( UNIT=LOUT, FMT=190 ) 'VARIANCE FACTOR               ', &
     &                                   STAT_VARFAC
           WRITE ( UNIT=LOUT, FMT=190 ) 'WRMS OF POSTFIT RESIDUALS     ', &
     &                                   STAT_WRMS
!
           WRITE ( UNIT=LOUT, FMT='(A)' ) '-SOLUTION/STATISTICS'
           WRITE ( LOUT, '(A)' ) '*'
           WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                           '---------------------------------------'
      END IF
!
! === SOLUTION/APRIORI section
!
      WRITE ( LOUT, '(A)' ) '*'
      WRITE ( LOUT, '(A)' ) '+SOLUTION/APRIORI'
      WRITE ( LOUT, '(A)' ) '*Index Type__ CODE PT SBIN Ref_epoch___ Unit '// &
     &                      'S Apriori_value________ Constraint_'
!
! --- First get several dates needed further
!
      IF ( FL_GLOBAL ) THEN
!
! ======== Global solution
!
! -------- Get the SAPDATE_STA --- reference date for station position
! -------- and velocities apriori
!
           CALL ERR_PASS ( IUER, IER )
           DATE_STA = JD_TO_DATE ( TIME0*YEAR__TO__DAY-32.184D0/864000.D0, IER ) 
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_SINEX ( DATE_STA, SAPDATE_STA, IER )
           IF ( IER .NE. 0 ) THEN
                WRITE ( 6, * ) ' TIME0*YEAR__TO__DAY = ', TIME0*YEAR__TO__DAY, &
     &                         ' (time in years for site epoch)'
                CALL ERR_LOG ( 3752, IUER, 'WRITE_SINEX', 'Trap of internal '// &
     &              'control: error in transformation the date '//DATE_STA// &
     &              ' to SINEX format' )
                RETURN
           END IF
!
! -------- Get the SINDATE_STA --- reference date for station position
! -------- and velocities estimates
!
           CALL ERR_PASS ( IUER, IER )
           DATE_STA = JD_TO_DATE ( SIT_EST_EPOCH - 32.184D0/864000.D0, IER ) 
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_SINEX ( DATE_STA, SINDATE_STA, IER )
           IF ( IER .NE. 0 ) THEN
                WRITE ( 6, * ) ' SIT_EST_EPOCH = ', SIT_EST_EPOCH, &
     &                         ' (time in years for site epoch)'
                CALL ERR_LOG ( 3753, IUER, 'WRITE_SINEX', 'Trap of internal '// &
     &              'control: error in transformation the date '//DATE_STA// &
     &              ' to SINEX format' )
                RETURN
           END IF
!
! -------- Get the SAPDATE_SRC --- reference date for source position and
! -------- proper motion apriori
!
! -------- NB! NB! NB! NB! NB! NB! NB! NB! NB! NB! NB! NB! NB! NB! NB!
! -------- Currently, 2002.12.24, there is no way to specity aprori
! -------- proper motion. Therefore, source position epoch is meaningless
!
           CALL ERR_PASS ( IUER, IER )
           DATE_SRC = JD_TO_DATE ( J2000__JD - 32.184D0/864000.D0, IER )
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_SINEX ( DATE_SRC, SAPDATE_SRC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3754, IUER, 'WRITE_SINEX', 'Trap of internal '// &
     &              'control: error in transformation the date '//DATE_SRC// &
     &              ' to SINEX format' )
                RETURN
           END IF
!
! -------- Get the SINDATE_SRC --- reference date for source position and
! -------- proper motion estiamtes
!
           CALL ERR_PASS ( IUER, IER )
           DATE_SRC = JD_TO_DATE ( SOU_EST_EPOCH - 32.184D0/864000.D0, IER )
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_SINEX ( DATE_SRC, SINDATE_SRC, IER )
           IF ( IER .NE. 0 ) THEN
                WRITE ( 6, * ) ' SOU_EST_EPOCH =', SOU_EST_EPOCH, &
     &                         ' (time in years since for source epoch)'
                CALL ERR_LOG ( 3755, IUER, 'WRITE_SINEX', 'Trap of internal '// &
     &              'control: error in transformation the date '//DATE_SRC// &
     &              ' to SINEX format' )
                RETURN
           END IF
!
! -------- Get the SINDATE_MID --- middle date of the global solution
!
           CALL ERR_PASS ( IUER, IER )
           DATE_MID = JD_TO_DATE ( (GLO_FJDOBS_MIN+GLO_FJDOBS_MAX)/2.0D0, IER )
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_SINEX ( DATE_MID, SINDATE_MID, IER )
           IF ( IER .NE. 0 ) THEN
                WRITE ( 6, * ) ' GLO_FJDOBS_MIN = ', GLO_FJDOBS_MIN, &
     &                         ' GLO_FJDOBS_MAX = ', GLO_FJDOBS_MAX
                CALL ERR_LOG ( 3756, IUER, 'WRITE_SINEX', 'Trap of internal '// &
     &              'control: error in transformation the date '//DATE_MID// &
     &              ' to SINEX format' )
                RETURN
           END IF
         ELSE
!
! ======== Local solution (this session only)
!
! -------- Get the middle date of the session. This will be the reference
! -------- date for nutation
!
           DATE_MID = JD_TO_DATE ( JDATE_ALL_MID, IER )
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_SINEX ( DATE_MID, SINDATE_MID, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3757, IUER, 'WRITE_SINEX', 'Trap of internal '// &
     &              'control: error in transformation the date '//DATE_MID// &
     &              ' to SINEX format' )
                RETURN
           END IF
!
! -------- Reference date for station and source position will be middle date
! -------- of this session -- it is obviuos, right?
!
           SINDATE_SRC = SINDATE_MID
           SINDATE_STA = SINDATE_STA
           SAPDATE_SRC = SINDATE_MID
           SAPDATE_STA = SINDATE_STA
!
! -------- Get the UT1 and pole coordinates reference date
!
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_SINEX ( JD_TO_DATE( TROT(1) - 32.184D0/86400.D0, IER ), &
     &                          SINDATE_EOP, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3758, IUER, 'WRITE_SINEX', 'Trap of internal '// &
     &              'control: error in transformation the date '// &
     &               JD_TO_DATE( TROT(1), IER )//' to SINEX format' )
                RETURN
           END IF
           SAPDATE_EOP = SINDATE_EOP
      END IF
!
! --- Cycle over all parameters. We have to collect apriories
!
      DO 4150 J15=1,L_SPR
         IF ( TYP_SPR(J15) .EQ. STX__SNX ) ICMP = 1
         IF ( TYP_SPR(J15) .EQ. STY__SNX ) ICMP = 2
         IF ( TYP_SPR(J15) .EQ. STZ__SNX ) ICMP = 3
!
         IF ( TYP_SPR(J15) .EQ. VEX__SNX ) ICMP = 1
         IF ( TYP_SPR(J15) .EQ. VEY__SNX ) ICMP = 2
         IF ( TYP_SPR(J15) .EQ. VEZ__SNX ) ICMP = 3
!
         IF ( TYP_SPR(J15) .EQ. RAS__SNX ) ICMP = 1
         IF ( TYP_SPR(J15) .EQ. DCL__SNX ) ICMP = 2
!
         IF ( TYP_SPR(J15) .EQ. RAR__SNX ) ICMP = 1
         IF ( TYP_SPR(J15) .EQ. DCR__SNX ) ICMP = 2
!
! ------ Get the index of this station (if it is a station-type parameter) in
!
! ------ ISITN_CHR list -- ISTA
! ------ STA_NAME  list -- ISTN
!
         STA_NAM_CLEAN = LPARM(IND_SPR(J15))(1:8) 
         CALL VTD_NAME_REPAIR ( STA_NAM_CLEAN )
         ISTA = LTM_DIF ( 1, INT4(NUMSTA), ISITN_CHR, LPARM(IND_SPR(J15))(1:8) )
         ISTN = LTM_DIF ( 1, L_STA, STA_NAME, STA_NAM_CLEAN )
         ISRC = LTM_DIF ( 1, INT4(NUMSTR), ISTRN_CHR, LPARM(IND_SPR(J15))(1:8) )
         IF ( ISTN .LE. 0                       .AND. &
     &        ( TYP_SPR(J15) .EQ. STX__SNX .OR. &
     &          TYP_SPR(J15) .EQ. STY__SNX .OR. &
     &          TYP_SPR(J15) .EQ. STZ__SNX .OR. &
     &          TYP_SPR(J15) .EQ. VEX__SNX .OR. &
     &          TYP_SPR(J15) .EQ. VEY__SNX .OR. &
     &          TYP_SPR(J15) .EQ. VEZ__SNX      )     ) THEN
!
              CALL ERR_LOG ( 3759, IUER, 'WRITE_SINEX', 'Trap of internal '// &
     &            'control: station '//STA_NAM_CLEAN//' was not found in '// &
     &            'the networking station table taken from the IVS data '// &
     &            'center ' )
              RETURN 
         END IF
         PAR_PT_NUM(J15) = 0
!
         STR_PT_NUM(IND_SPR(J15)) = '   1'
         CALL CHIN ( LPARM(IND_SPR(J15))(11:16), IEPOCH )
         IF ( FL_GLOBAL                                 .AND. &
     &        IEPOCH .GT. 0  .AND.  IEPOCH .LT. 999999  .AND. &
     &        ( LPARM(IND_SPR(J15))(17:20) .EQ. '-POS'  .OR. &
     &          LPARM(IND_SPR(J15))(17:20) .EQ. '-COO'        )  ) THEN
!
              DO 4160 J16=1,NUMSTA
                 IF ( ISITN_CHR(J16) .EQ. LPARM(IND_SPR(J15))(1:8) .AND. &
     &                PT_DAT(J16) .EQ. LPARM(IND_SPR(J15))(11:16)        ) THEN
                      PAR_PT_NUM(IND_SPR(J15)) = STA_PARIND(J16)+1
                 END IF
 4160         CONTINUE
              IF ( PAR_PT_NUM(IND_SPR(J15)) .LE. 0 ) THEN
                   CALL ERR_LOG ( 3760, IUER, 'WRITE_SINEX', 'Trap of '// &
     &                 'internal control: parameter '//LPARM(IND_SPR(J15))// &
     &                 ' was lost' )
                   RETURN
              END IF
              CALL INCH   ( PAR_PT_NUM(IND_SPR(J15)), &
     &                      STR_PT_NUM(IND_SPR(J15))(1:4) )
              CALL CHASHR ( STR_PT_NUM(IND_SPR(J15))(1:4)  )
         END IF
!
         IF ( TYP_SPR(J15) .EQ. STX__SNX .OR. &
     &        TYP_SPR(J15) .EQ. STY__SNX .OR. &
     &        TYP_SPR(J15) .EQ. STZ__SNX      ) THEN
!
! ----------- A component of station position
!
              IF ( ISTA .LE. 0 ) THEN
                   CALL ERR_LOG ( 3761, IUER, 'WRITE_SINEX', 'Trap of '// &
     &                 'internal control. Parameter: '//LPARM(IND_SPR(J15)) )
                   RETURN
              END IF
!
! ----------- WARNING: actual value of constraint is ignored
!
              IF ( FL_GLOBAL ) THEN
                   WRITE ( UNIT=LOUT, FMT=120 ) J15, NAME__SNX(TYP_SPR(J15)), &
     &                     STA_CDP(ISTN), STR_PT_NUM(IND_SPR(J15)), &
     &                     SAPDATE_STA, UNIT__SNX(TYP_SPR(J15)), &
     &                     VSITEC(ICMP,ISTA), 0.0D0
                 ELSE
                   WRITE ( UNIT=LOUT, FMT=120 ) J15, NAME__SNX(TYP_SPR(J15)), &
     &                     STA_CDP(ISTN), '   1', STA_DATE_MID(ISTA), &
     &                     UNIT__SNX(TYP_SPR(J15)), NVSITEC(ICMP,ISTA), 0.0D0
              END IF
 120          FORMAT ( 1X,I5, 1X,A6, 1X,I4, 1X,' A', 1X, A4, 1X,A12, 1X,A4, &
     &                 ' 2', 1X,1PD21.14, 1X,1PD11.5 )
!
           ELSE IF ( TYP_SPR(J15) .EQ. VEX__SNX .OR. &
     &               TYP_SPR(J15) .EQ. VEY__SNX .OR. &
     &               TYP_SPR(J15) .EQ. VEZ__SNX      ) THEN
!
! ----------- A component of station velocity
!
              IF ( ISTA .LE. 0 ) THEN
                   CALL ERR_LOG ( 3762, IUER, 'WRITE_SINEX', 'Trap of '// &
     &                 'internal control. Parameter: '//LPARM(IND_SPR(J15)) )
                   RETURN
              END IF
!
! ----------- WARNING: actual value of constraint is ignored
!
              WRITE ( UNIT=LOUT, FMT=120 ) J15, NAME__SNX(TYP_SPR(J15)), &
     &                STA_CDP(ISTN), '   1', SAPDATE_STA, &
     &                UNIT__SNX(TYP_SPR(J15)), VSITEV(ICMP,ISTA), 0.0D0
!
           ELSE IF ( TYP_SPR(J15) .EQ. AXI__SNX ) THEN
              ISTA = LTM_DIF ( 1, INT4(NUMSTA), ISITN_CHR, &
     &                         LPARM(IND_SPR(J15))(1:8))
              ISTN = LTM_DIF ( 1, L_STA, STA_NAME, LPARM(IND_SPR(J15))(1:8) )
!
! ----------- Antenna axis offset
!
              IF ( ISTA .LE. 0 ) THEN
                   CALL ERR_LOG ( 3763, IUER, 'WRITE_SINEX', 'Trap of '// &
     &                 'internal control. Parameter: '//LPARM(IND_SPR(J15)) )
                   RETURN
              END IF
!
! ----------- WARNING: actual value of constraint is ignored
!
              WRITE ( UNIT=LOUT, FMT=120 ) J15, NAME__SNX(TYP_SPR(J15)), &
     &                STA_CDP(ISTN), '   1', SAPDATE_STA, &
     &                UNIT__SNX(TYP_SPR(J15)), VAXOF(ISTA), 0.0D0
           ELSE IF ( TYP_SPR(J15) .EQ. RAS__SNX  .OR. &
     &               TYP_SPR(J15) .EQ. DCL__SNX       ) THEN
!
! ----------- Source coordinates
!
              IF ( ISRC .LE. 0 ) THEN
                   CALL ERR_LOG ( 3764, IUER, 'WRITE_SINEX', 'Trap of '// &
     &                 'internal control. Parameter: '//LPARM(IND_SPR(J15)) )
                   RETURN
              END IF
              IF ( FL_GLOBAL ) THEN
                   WRITE ( UNIT=LOUT, FMT=125 ) J15, NAME__SNX(TYP_SPR(J15)), &
     &                     SOURCE_ID(ISRC), CHAR26(0), SAPDATE_SRC, &
     &                     UNIT__SNX(TYP_SPR(J15)), VSTARC(ICMP,ISRC), 0.0D0
                 ELSE
                   WRITE ( UNIT=LOUT, FMT=125 ) J15, NAME__SNX(TYP_SPR(J15)), &
     &                     SOURCE_ID(ISRC), CHAR26(0), SRC_DATE_MID(ISRC), &
     &                     UNIT__SNX(TYP_SPR(J15)), VSTARC(ICMP,ISRC), 0.0D0
              END IF
 125          FORMAT ( 1X,I5, 1X,A6, 1X,A4, 1X,A, '    1', 1X,A12, 1X,A4, &
     &                 ' 2', 1X,1PD21.14, 1X,1PD11.5 )
           ELSE IF ( TYP_SPR(J15) .EQ. RAR__SNX  .OR. &
     &               TYP_SPR(J15) .EQ. DCR__SNX       ) THEN
!
! ----------- Source's proper motion
!
              IF ( ISRC .LE. 0 ) THEN
                   CALL ERR_LOG ( 3765, IUER, 'WRITE_SINEX', 'Trap of '// &
     &                 'internal control. Parameter: '//LPARM(IND_SPR(J15)) )
                   RETURN
              END IF
!
              WRITE ( UNIT=LOUT, FMT=125 ) J15, NAME__SNX(TYP_SPR(J15)), &
     &                SOURCE_ID(ISRC), CHAR26(0), SAPDATE_SRC, &
     &                UNIT__SNX(TYP_SPR(J15)), 0.0D0, 0.0D0
           ELSE IF ( TYP_SPR(J15) .EQ. XPL__SNX .OR. &
     &               TYP_SPR(J15) .EQ. YPL__SNX .OR. &
     &               TYP_SPR(J15) .EQ. UT1__SNX .OR. &
     &               TYP_SPR(J15) .EQ. XPR__SNX .OR. &
     &               TYP_SPR(J15) .EQ. YPR__SNX .OR. &
     &               TYP_SPR(J15) .EQ. UTR__SNX      ) THEN
!
! =========== WARNING: apriori sigmas are always reported as zero, since
! -----------          equations of constraints are provided in constraint
! -----------          section. Setting constraints in Solve is more
! -----------          sophisitcated than in GPS analysis software
!
              WRITE ( UNIT=LOUT, FMT=130 ) J15, NAME__SNX(TYP_SPR(J15)), &
     &                SAPDATE_EOP, UNIT__SNX(TYP_SPR(J15)), &
     &                APR_VAL(TYP_SPR(J15))*SCAL__SNX(TYP_SPR(J15)), &
     &                APR_SIG(TYP_SPR(J15))*SCAL__SNX(TYP_SPR(J15))
 130          FORMAT ( 1X,I5, 1X,A6, ' ----  -    1', 1X,A12, 1X,A4, &
     &                 ' 2', 1X,1PD21.14, 1X,1PD11.5 )
           ELSE IF ( TYP_SPR(J15) .EQ. PSI__SNX .OR. &
     &               TYP_SPR(J15) .EQ. EPS__SNX .OR. &
     &               TYP_SPR(J15) .EQ. NTX__SNX .OR. &
     &               TYP_SPR(J15) .EQ. NTY__SNX      ) THEN
              WRITE ( UNIT=LOUT, FMT=130 ) J15, NAME__SNX(TYP_SPR(J15)), &
     &                SINDATE_MID, UNIT__SNX(TYP_SPR(J15)), &
     &                APR_VAL(TYP_SPR(J15))*SCAL__SNX(TYP_SPR(J15)), &
     &                APR_SIG(TYP_SPR(J15))*SCAL__SNX(TYP_SPR(J15))
         END IF
 4150 CONTINUE
!
      WRITE ( LOUT, '(A)' ) '-SOLUTION/APRIORI'
      WRITE ( LOUT, '(A)' ) '*'
      WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                      '---------------------------------------'
      WRITE ( LOUT, '(A)' ) '*'
!
! === SOLUTION/ESTIMATE block
!
      WRITE ( LOUT, '(A)' ) '+SOLUTION/ESTIMATE'
      WRITE ( LOUT, '(A)' ) '*Index TYPE__ CODE PT SBIN Ref_epoch___ Unit '// &
     &                      'S Total_value__________ Formal_erro'
      DO 4170 J17=1,L_SPR
!
! ------ Get indexes of the parameters in station list and in station table
!
         STA_NAM_CLEAN = LPARM(IND_SPR(J17))(1:8) 
         CALL VTD_NAME_REPAIR ( STA_NAM_CLEAN )
         ISTA = LTM_DIF ( 1, INT4(NUMSTA), ISITN_CHR, LPARM(IND_SPR(J17))(1:8))
         ISTN = LTM_DIF ( 1, L_STA, STA_NAME, STA_NAM_CLEAN )
         ISRC = LTM_DIF ( 1, INT4(NUMSTR), ISTRN_CHR, LPARM(IND_SPR(J17))(1:8))
         IF ( ISTN .LE. 0                       .AND. &
     &        ( TYP_SPR(J17) .EQ. STX__SNX .OR. &
     &          TYP_SPR(J17) .EQ. STY__SNX .OR. &
     &          TYP_SPR(J17) .EQ. STZ__SNX .OR. &
     &          TYP_SPR(J17) .EQ. VEX__SNX .OR. &
     &          TYP_SPR(J17) .EQ. VEY__SNX .OR. &
     &          TYP_SPR(J17) .EQ. VEZ__SNX      )     ) THEN
              CALL ERR_LOG ( 3766, IUER, 'WRITE_SINEX', 'Trap of internal '// &
     &            'control: station '//STA_NAM_CLEAN//' was not found in '// &
     &            'the networking station table taken from the IVS data '// &
     &            'center ' )
              RETURN 
         END IF
!
         IF ( TYP_SPR(J17) .EQ. STX__SNX ) ICMP = 1
         IF ( TYP_SPR(J17) .EQ. STY__SNX ) ICMP = 2
         IF ( TYP_SPR(J17) .EQ. STZ__SNX ) ICMP = 3
!
         IF ( TYP_SPR(J17) .EQ. VEX__SNX ) ICMP = 1
         IF ( TYP_SPR(J17) .EQ. VEY__SNX ) ICMP = 2
         IF ( TYP_SPR(J17) .EQ. VEZ__SNX ) ICMP = 3
!
         IF ( TYP_SPR(J17) .EQ. RAS__SNX ) ICMP = 1
         IF ( TYP_SPR(J17) .EQ. DCL__SNX ) ICMP = 2
!
         IF ( TYP_SPR(J17) .EQ. RAR__SNX ) ICMP = 1
         IF ( TYP_SPR(J17) .EQ. DCR__SNX ) ICMP = 2
!
         IF ( TYP_SPR(J17) .EQ. STX__SNX .OR. &
     &        TYP_SPR(J17) .EQ. STY__SNX .OR. &
     &        TYP_SPR(J17) .EQ. STZ__SNX       ) THEN
!
! ----------- Station coordaitnes
!
              IF ( ISTA .LE. 0 ) THEN
                   CALL ERR_LOG ( 3767, IUER, 'WRITE_SINEX', 'Trap of '// &
     &                 'internal control: station was lost' )
                   RETURN
              END IF
              IF ( FL_GLOBAL ) THEN
                   WRITE ( UNIT=LOUT, FMT=120 ) J17, NAME__SNX(TYP_SPR(J17)), &
     &                     STA_CDP(ISTN), STR_PT_NUM(IND_SPR(J17)), &
     &                     SINDATE_STA, UNIT__SNX(TYP_SPR(J17)), &
     &                     (VSITEC(ICMP,ISTA)+EST_VEC(IND_SPR(J17)))* &
     &                     SCAL__SNX(TYP_SPR(J17)), &
     &                     SIG_VEC(IND_SPR(J17))*DABS(SCAL__SNX(TYP_SPR(J17)))
                 ELSE
                   WRITE ( UNIT=LOUT, FMT=120 ) J17, NAME__SNX(TYP_SPR(J17)), &
     &                     STA_CDP(ISTN), '   1', &
     &                     STA_DATE_MID(ISTA), UNIT__SNX(TYP_SPR(J17)), &
     &                     ( NVSITEC(ICMP,ISTA) + &
     &                       EST_VEC(IND_SPR(J17)))*SCAL__SNX(TYP_SPR(J17)), &
     &                     SIG_VEC(IND_SPR(J17))*DABS(SCAL__SNX(TYP_SPR(J17)))
              END IF
!
              ADJ_VEC(J17) = EST_VEC(IND_SPR(J17))*SCAL__SNX(TYP_SPR(J17))
!
           ELSE IF ( TYP_SPR(J17) .EQ. VEX__SNX .OR. &
     &               TYP_SPR(J17) .EQ. VEY__SNX .OR. &
     &               TYP_SPR(J17) .EQ. VEZ__SNX       ) THEN
!
! ----------- Station velocity
!
              IF ( ISTA .LE. 0 ) THEN
                   CALL ERR_LOG ( 3768, IUER, 'WRITE_SINEX', 'Trap of '// &
     &                 'internal control: station was lost' )
                   RETURN
              END IF
              WRITE ( UNIT=LOUT, FMT=120 ) J17, NAME__SNX(TYP_SPR(J17)), &
     &                STA_CDP(ISTN), '   1', SINDATE_STA, &
     &                UNIT__SNX(TYP_SPR(J17)), &
     &                (VSITEV(ICMP,ISTA)+EST_VEC(IND_SPR(J17)))* &
     &                SCAL__SNX(TYP_SPR(J17)), &
     &                SIG_VEC(IND_SPR(J17))*DABS(SCAL__SNX(TYP_SPR(J17)))
              ADJ_VEC(J17) = EST_VEC(IND_SPR(J17))*SCAL__SNX(TYP_SPR(J17))
           ELSE IF ( TYP_SPR(J17) .EQ. XPL__SNX .OR. &
     &               TYP_SPR(J17) .EQ. YPL__SNX .OR. &
     &               TYP_SPR(J17) .EQ. UT1__SNX .OR. &
     &               TYP_SPR(J17) .EQ. XPR__SNX .OR. &
     &               TYP_SPR(J17) .EQ. YPR__SNX .OR. &
     &               TYP_SPR(J17) .EQ. UTR__SNX      ) THEN
!
              WRITE ( UNIT=LOUT, FMT=130 ) J17, NAME__SNX(TYP_SPR(J17)), &
     &                SINDATE_EOP, UNIT__SNX(TYP_SPR(J17)), &
     &                ( APR_VAL(TYP_SPR(J17)) + EST_VAL(TYP_SPR(J17)) )* &
     &                                          SCAL__SNX(TYP_SPR(J17)), &
     &                DSQRT ( COV_MAT(LOCC(IND_SPR(J17),IND_SPR(J17))) )* &
     &                DABS(SCAL__SNX(TYP_SPR(J17)))
              ADJ_VEC(J17) = EST_VEC(IND_SPR(J17))*SCAL__SNX(TYP_SPR(J17))
           ELSE IF ( TYP_SPR(J17) .EQ. PSI__SNX .OR. &
     &               TYP_SPR(J17) .EQ. EPS__SNX .OR. &
     &               TYP_SPR(J17) .EQ. NTX__SNX .OR. &
     &               TYP_SPR(J17) .EQ. NTY__SNX      ) THEN
!
! ----------- Nutation
!
              WRITE ( UNIT=LOUT, FMT=130 ) J17, NAME__SNX(TYP_SPR(J17)), &
     &                SINDATE_MID, UNIT__SNX(TYP_SPR(J17)), &
     &                EST_VAL(TYP_SPR(J17))*SCAL__SNX(TYP_SPR(J17)), &
     &                SIG_VEC(IND_SPR(J17))*DABS(SCAL__SNX(TYP_SPR(J17)))
              ADJ_VEC(J17) = EST_VEC(IND_SPR(J17))*SCAL__SNX(TYP_SPR(J17))
           ELSE IF ( TYP_SPR(J17) .EQ. RAS__SNX .OR. &
     &               TYP_SPR(J17) .EQ. DCL__SNX      ) THEN
!
! ----------- Source coordinates
!
              IF ( FL_GLOBAL ) THEN
                   WRITE ( UNIT=LOUT, FMT=135 ) J17, NAME__SNX(TYP_SPR(J17)), &
     &                   SOURCE_ID(ISRC), SINDATE_SRC, &
     &                   UNIT__SNX(TYP_SPR(J17)),(VSTARC(ICMP,ISRC) + EST_VEC(IND_SPR(J17)))* &
     &                   SCAL__SNX(TYP_SPR(J17)), &
     &                   SIG_VEC(IND_SPR(J17))*DABS(SCAL__SNX(TYP_SPR(J17)))
                 ELSE
                   WRITE ( UNIT=LOUT, FMT=135 ) J17, NAME__SNX(TYP_SPR(J17)), &
     &                   SOURCE_ID(ISRC), SRC_DATE_MID(ISRC), &
     &                   UNIT__SNX(TYP_SPR(J17)), &
     &                   (VSTARC(ICMP,ISRC) + EST_VEC(IND_SPR(J17)))* &
     &                   SCAL__SNX(TYP_SPR(J17)), &
     &                   SIG_VEC(IND_SPR(J17))*DABS(SCAL__SNX(TYP_SPR(J17)))
              END IF
 135          FORMAT ( 1X,I5, 1X,A6, 1X,A4, '  A    1', 1X,A12, 1X,A4, &
     &                 ' 2', 1X,1PD21.14, 1X,1PD11.5 )
              ADJ_VEC(J17) = EST_VEC(IND_SPR(J17))*SCAL__SNX(TYP_SPR(J17))
           ELSE IF ( TYP_SPR(J17) .EQ. AXI__SNX       ) THEN
!
! ----------- Axis offset
!
              WRITE ( UNIT=LOUT, FMT=120 ) J17, NAME__SNX(TYP_SPR(J17)), &
     &                STA_CDP(ISTN), '   1', SINDATE_MID, &
     &                UNIT__SNX(TYP_SPR(J17)), &
     &                (VAXOF(ISTA) + EST_VEC(TYP_SPR(J17)))* &
     &                SCAL__SNX(TYP_SPR(J17)), &
     &                SIG_VEC(IND_SPR(J17))*DABS(SCAL__SNX(TYP_SPR(J17)))
              ADJ_VEC(J17) = EST_VEC(IND_SPR(J17))*SCAL__SNX(TYP_SPR(J17))
           ELSE IF ( TYP_SPR(J17) .EQ. RAR__SNX  .OR. &
     &               TYP_SPR(J17) .EQ. DCR__SNX       ) THEN
!
! ----------- Proper motion
!
              WRITE ( UNIT=LOUT, FMT=135 ) J17, NAME__SNX(TYP_SPR(J17)), &
     &                SOURCE_ID(ISRC), SINDATE_SRC, UNIT__SNX(TYP_SPR(J17)), &
     &                EST_VEC(IND_SPR(J17))*SCAL__SNX(TYP_SPR(J17)), &
     &                SIG_VEC(IND_SPR(J17))*DABS(SCAL__SNX(TYP_SPR(J17)))
              ADJ_VEC(J17) = EST_VEC(IND_SPR(J17))*SCAL__SNX(TYP_SPR(J17))
         END IF
 4170 CONTINUE
!
      WRITE ( LOUT, '(A)' ) '-SOLUTION/ESTIMATE'
      WRITE ( LOUT, '(A)' ) '*'
      WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                      '---------------------------------------'
      WRITE ( LOUT, '(A)' ) '*'
!
! --- Read constraint equations from scratch file
!
      CALL ERR_PASS ( IUER, IER )
      IF ( KGLOBALS ) THEN
           CALL READ_CNSTR ( CNSTROBJ, CNI__GLO, IER )
         ELSE
           CALL READ_CNSTR ( CNSTROBJ, CNI__LOC, IER )
      END IF
!
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3769, IUER, 'WRITE_SINEX', 'Error in an attempt '// &
     &         'to read equation of consrtaints from the scratch file' )
           RETURN
      END IF
!
! --- Compute amount of dynamic memory for holding the matrix of constraint
! --- equations
!
      IF ( FL_SINEX_DCM ) THEN
           LEN_NOR_DCM = 8*(L_SPR*(L_SPR+1))/2
           LEN_NRV_DCM = 8*L_SPR
           LEN_CNS_MAT = 8*L_SPR*CNSTROBJ%N_EQUAT
           LEN_CNS_WMA = 8*L_SPR*CNSTROBJ%N_EQUAT
           LEN_CNS_NRM = 8*(L_SPR*(L_SPR+1))
      END IF
      IF ( FL_SINEX_DCM  .OR.  FL_SINEX_CNS ) THEN
           LEN_WEI_CNS = 8*(CNSTROBJ%N_EQUAT*(CNSTROBJ%N_EQUAT+1))/2
      END IF
!
      IF ( FL_SINEX_CNS  .OR.  FL_SINEX_DCM ) THEN
!
! -------- Grab dynamic memory for holding matrix of constraint equations and
! -------- related matrices
!
           CALL ERR_PASS ( IUER, IER )
           CALL GRAB_MEM ( IER, MEM_LEN,           MEM_ADR,     6, &
     &                          INT8(LEN_NOR_DCM), ADR_NOR_DCM, &
     &                          INT8(LEN_NRV_DCM), ADR_NRV_DCM, &
     &                          INT8(LEN_CNS_MAT), ADR_CNS_MAT, &
     &                          INT8(LEN_CNS_MAT), ADR_CNS_WMA, &
     &                          INT8(LEN_CNS_NRM), ADR_CNS_NRM, &
     &                          INT8(LEN_WEI_CNS), ADR_WEI_CNS     )
           IF ( IER .NE. 0 ) THEN
                CALL IINCH   ( MEM_LEN, STR )
                CALL ERR_LOG ( 3770, IUER, 'WRITE_SINEX', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for weight matrix of constraints' )
                RETURN
           END IF
!
! -------- Initialization
!
           IF ( FL_SINEX_DCM ) THEN
                CALL NOUT ( LEN_NOR_DCM, %VAL(ADR_NOR_DCM) )
                CALL NOUT ( LEN_NRV_DCM, %VAL(ADR_NRV_DCM) )
                CALL NOUT ( LEN_CNS_MAT, %VAL(ADR_CNS_MAT) )
                CALL NOUT ( LEN_CNS_WMA, %VAL(ADR_CNS_WMA) )
                CALL NOUT ( LEN_CNS_NRM, %VAL(ADR_CNS_NRM) )
           END IF
           CALL NOUT ( LEN_WEI_CNS, %VAL(ADR_WEI_CNS) )
      END IF
!
! --- Continuation of the work: writing the matrix of constraint equations,
! --- the weight matrix of constraints, computing and writing down
! --- decomposed normal matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRITE_SINEX_CONT ( L_SPR, CNSTROBJ, COV_MAT, ADJ_VEC, &
     &                        %VAL(ADR_NOR_DCM), %VAL(ADR_NRV_DCM), &
     &                        %VAL(ADR_CNS_MAT), %VAL(ADR_CNS_NRM), &
     &                        %VAL(ADR_CNS_WMA), %VAL(ADR_WEI_CNS), &
     &                        TYP_SPR, IND_SPR, LOUT, IER  )
      CALL FREE_MEM ( MEM_ADR )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3771, IUER, 'WRITE_SINEX', 'Error in '// &
     &                   'WRITE_SINEX_CONT' )
           RETURN
      END IF
!
      IF ( .NOT. FL_GLOBAL ) THEN
!
! -------- Write down the trailing line of sinex listing and that is.
! -------- Deal done if we in LOCAL mode
!
           WRITE ( LOUT , FMT='(A)' ) '%ENDSNX'
      END IF
      CLOSE ( UNIT=LOUT )
!
      IF ( SINEX_VERS(1:4) .EQ. '2.20' ) THEN
           SED_FILE = PRE_SAV_DIR(1:PRE_SV_LEN)// &
     &                SINEX_220_SED(1:I_LEN(SINEX_220_SED))
           INQUIRE ( FILE=SED_FILE, EXIST=LEX ) 
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 3772, IUER, 'WRITE_SINEX', 'Cannot find '// &
     &              'file '//SED_FILE(1:I_LEN(SED_FILE))//' . Strange, '// &
     &              'this file should be there. If you acidentally lost it, '// &
     &              'grap it from http://vlbi.gsfc.nasa.gov/solve ' )
                RETURN
           END IF
!
           CALL GETENVAR ( 'SOLVE_SED', COM_STR )
           IF ( ILEN(COM_STR) == 0 ) COM_STR = 'sed '
           COM_STR = COM_STR(1:I_LEN(COM_STR))//' -i -f '// &
     &               SED_FILE(1:I_LEN(SED_FILE))//' '//SINEX_REALNAME
           IS = SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
!
! ------------- Completion code is not 0. Extract ISIG -- signal number which
! ------------- caused termination of the command and ICOD -- completion code
!
                ISIG = 0
                ICOD = 0
                CALL MVBITS ( IS, 0, 8, ISIG, 0 )
                CALL MVBITS ( IS, 8, 8, ICOD, 0 )
                IF ( ICOD .GE. 128 ) ICOD = ICOD-256
!
! ------------- Try to diagnose the reason
!
                IF ( ICOD .EQ. -1 ) THEN
                     CALL ERR_LOG ( 3773, IUER, 'WRITE_SINEX', 'Error in '// &
     &                   'excuting command file '//COM_STR(1:I_LEN(COM_STR))// &
     &                   ' failed' )
                     RETURN
                END IF
!
! ------------- We failed to find a reason...
!
                WRITE ( 6, * ) ' is=',is,' isig=',isig,' icod=',icod ! %%%%%%
                CALL ERR_LOG ( 5175, IUER, 'EXECUTE_COMMAND', 'Error in '// &
     &              'excuting command '//COM_STR)
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  WRITE_SINEX  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE WRITE_SINEX_CONT ( L_SPR, CNSTROBJ, COV_MAT, ADJ_VEC, &
     &           NOR_DCM, NRV_DCM, CNS_MAT, CNS_WMA, CNS_NRM, WEI_CNS, &
     &           TYP_SPR, IND_SPR, LOUT, IUER  )
! ************************************************************************
! *                                                                      *
! *     Routine WRITE_SINEX_CONT is a continuation of WRITE_SINEX.       *
! *   It makes the second part of the job. Code is split on two          *
! *   subroutines because of limitations of Fortran77 on work with       *
! *   dynamic memory. This routine writes down the covariance matrix     *
! *   of the estiamtes, the matrix of constraint equations, the vector   *
! *   of right-hand side of the constraitn equatios and the reciprocal   *
! *   weights. It also writes down the weight matrix of constraints,     *
! *   computes and writes down decomposed normal matrix.                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     L_SPR ( RECORD    ) -- The number of parameters which are put    *
! *                            in Sinex listing.                         *
! *  CNSTROBJ ( RECORD    ) -- The data structure with information       *
! *                            about equations of constraints.           *
! *   COV_MAT ( REAL*8    ) -- Full covariance matrix.                   *
! *   ADJ_VEC ( REAL*8    ) -- Vector of adjustments of the parameters   *
! *                            which are put in Sinex listing.           *
! *      LOUT ( INTEGER*4 ) -- Fortran logical unit I/O opened for       *
! *                            Sinex output file.                        *
! *   TYP_SPR ( INTEGER*4 ) -- Array of types of parameters which are    *
! *                            put in Sinex listing. Types are defined   *
! *                            in glbc4.i                                *
! *   IND_SPR ( INTEGER*4 ) -- Array of cross-reference indices.         *
! *                            IND_SPR(I) is and index in full           *
! *                            covariance matrix for the I-th parameter  *
! *                            which is put in Sinex listing.            *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   NOR_DCM ( REAL*8    ) -- Decomposed matrix of normal equations.    *
! *   NRV_DCM ( REAL*8    ) -- Decomposed vector of normal equations.    *
! *   CNS_MAT ( REAL*8    ) -- Matrix of constraints.                    *
! *   CNS_WMA ( REAL*8    ) -- Some intermediate matrix.                 *
! *   CNS_NRM ( REAL*8    ) -- Normal weighted matrix of constraints.    *
! *   WEI_CNS ( REAL*8    ) -- Weight matrix of constraints in upper     *
! *                            triangular representation.                *
! *                            Dimension: CNSTROBJ.N_EQUAT               *
! *                                                                      *
! * ### 08-MAY-2002  WRITE_SINEX_CONT v1.1 (c) L. Petrov 13-MAY-2002 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'cnstr.i'
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*4  L_SPR, LOUT, IUER
      REAL*8     COV_MAT(*), ADJ_VEC(*), NOR_DCM(*), NRV_DCM(*), CNS_MAT(*), &
     &           CNS_WMA(*), CNS_NRM(*), WEI_CNS(*)
      INTEGER*4  IND_SPR(M_GPA), TYP_SPR(M_GPA)
!
      REAL*8       SMALL_EPS
      PARAMETER  ( SMALL_EPS = 1.D-14 )
      REAL*8     RC
      INTEGER*4  IEQU_COU, ITP, ITP1, ITP2, IP, IER
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12
      INTEGER*4  I, J, MV, LTM_DIF, IFIND_PL
      INTEGER*8  LOCC, LOCR
      LOCC(I,J) = INT8(MIN(I,J)) + (INT8(MAX(I,J))*INT8(MAX(I,J)-1))/2
      LOCR(I,J,MV) = ( INT8(MV)*INT8(I-1) + J  )
!
      IF ( FL_SINEX_COV ) THEN
!
! ======== SOLUTION/MATRIX_ESTIMATE block
!
           WRITE ( LOUT, '(A)' ) '+SOLUTION/MATRIX_ESTIMATE L COVA'
           WRITE ( LOUT, '(A)' ) '* Ind1  Ind2 Covariance(ind1,ind2)'
       END IF
!
       DO 410 J1=1,L_SPR
          DO 420 J2=1,J1
             IF ( FL_SINEX_COV ) THEN
                  WRITE ( UNIT=LOUT, FMT=110 ) J1, J2, &
     &                   COV_MAT(LOCC(IND_SPR(J1), &
     &                 IND_SPR(J2)))*( SCAL__SNX(TYP_SPR(J1))*SCAL__SNX(TYP_SPR(J2)) )
 110              FORMAT ( 1X,I5, 1X,I5, 1X,1PD21.14 )
             END IF
             IF ( FL_SINEX_DCM ) THEN
!
! --------------- Fill the elements of NOR_DCM matrix. At this stage it keeps
! --------------- the elements of the partial covariance matrix of the estiamtes
!
                  NOR_DCM( LOCC(J1,J2) ) = &
     &                     COV_MAT(LOCC(IND_SPR(J1), &
     &                    IND_SPR(J2)))*( SCAL__SNX(TYP_SPR(J1))*SCAL__SNX(TYP_SPR(J2)) )
             END IF
 420      CONTINUE
 410   CONTINUE
!
      IF ( FL_SINEX_COV ) THEN
           WRITE ( LOUT, '(A)' ) '-SOLUTION/MATRIX_ESTIMATE L COVA'
           WRITE ( LOUT, '(A)' ) '*'
           WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                           '---------------------------------------'
           WRITE ( LOUT, '(A)' ) '*'
      END IF
!
      IF ( FL_SINEX_CNS ) THEN
!
! ======== SOLUTION/CONSTRAINT_INFO block
!
           WRITE ( UNIT=LOUT, FMT='(A)' ) '+SOLUTION/CONSTRAINT_INFO'
           WRITE ( UNIT=LOUT, FMT='(A)' ) '*Index Cns_abr  Subind Unit  '// &
     &                                    'Description_of_constraint'
           IEQU_COU = 0
           DO 430 J3=1,CNSTROBJ%N_EQUAT
!
! ----------- Find the index of the constraint in NAME__CNS list. We need to
! ----------- learn units used for Sinex output
!
              ITP = LTM_DIF ( 1, N__CNS, NAME__CNS, CNSTROBJ%ABB_CNS(J3) )
              IF ( CNSTROBJ%USR_CNS(J3) ) THEN
!
! ---------------- User constraints cannot be found in NAME__CNS. We also be
! ---------------- reporting user defined units
!
                   IEQU_COU  = IEQU_COU + 1
                   WRITE ( UNIT=LOUT, FMT=120 ) IEQU_COU, &
     &                     CNSTROBJ%ABB_CNS(J3), CNSTROBJ%SBI_CNS(J3), &
     &                     CNSTROBJ%UNT_CNS(J3)(1:4), CNSTROBJ%DSC_CNS(J3)
                 ELSE
                   IF ( ITP .GT. 0 ) THEN
                        IEQU_COU  = IEQU_COU + 1
                        WRITE ( UNIT=LOUT, FMT=120 ) IEQU_COU, &
     &                          CNSTROBJ%ABB_CNS(J3), CNSTROBJ%SBI_CNS(J3), &
     &                          UNIT__CNS(ITP), CNSTROBJ%DSC_CNS(J3)
 120                    FORMAT ( 1X,I5, 1X,A8, 2X,I5, 1X,A, 2X,A )
                      ELSE
                        CALL ERR_LOG ( 3772, IUER, 'WRITE_SINEX_CONT', &
     &                      'Trap of internal control: constraint '// &
     &                       CNSTROBJ%ABB_CNS(J3)//' was not found in the '// &
     &                      'list of supported constraint abbreviations '// &
     &                      'NAME__CNS defined in glbc4.i' )
                        RETURN
                   END IF
              END IF
 430      CONTINUE
!
           WRITE ( UNIT=LOUT, FMT='(A)' ) '-SOLUTION/CONSTRAINT_INFO'
           WRITE ( LOUT, '(A)' ) '*'
           WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                           '---------------------------------------'
           WRITE ( LOUT, '(A)' ) '*'
!
! ======== SOLUTION/CONSTRAINT_EQUATION_MATRIX block
!
           WRITE ( UNIT=LOUT, FMT='(A)' ) '+SOLUTION/CONSTRAINT_EQUATION_MATRIX'
           WRITE ( UNIT=LOUT, FMT='(A)' ) '* IND1  runs over constaint '// &
     &                                    'equations'
           WRITE ( UNIT=LOUT, FMT='(A)' ) '* IND2  runs over parameters '// &
     &                                    'to be constrainted'
           WRITE ( UNIT=LOUT, FMT='(A)' ) '*'
           WRITE ( UNIT=LOUT, FMT='(A)' ) '* Ind1  Ind2 '// &
     &                                    'Constraint_equation(ind1,ind2)'
      END IF
!
      DO 440 J4=1,CNSTROBJ%N_ECNST
         IP  = IFIND_PL ( L_SPR, IND_SPR, CNSTROBJ%EQU_INP(J4) )
         IF ( IP .GT. 0 ) THEN
              IF ( FL_SINEX_DCM ) THEN
!
! ---------------- Collect elements of constraint matrix
!
                   CNS_MAT( LOCR(IP, CNSTROBJ%EQU_INE(J4), CNSTROBJ%N_EQUAT) ) = &
     &                      CNSTROBJ%EQU_CNS(J4)
              END IF
              IF ( FL_SINEX_CNS ) THEN
                   WRITE ( UNIT=LOUT, FMT=130 ) CNSTROBJ%EQU_INE(J4), IP, &
     &                                          CNSTROBJ%EQU_CNS(J4)
 130               FORMAT ( 1X,I5, 1X,I5, 1X,1PD21.14 )
              END IF
         END IF
 440  CONTINUE
!
      IF ( FL_SINEX_CNS ) THEN
           WRITE ( UNIT=LOUT, FMT='(A)' ) '-SOLUTION/CONSTRAINT_EQUATION_MATRIX'
           WRITE ( LOUT, '(A)' ) '*'
           WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                           '---------------------------------------'
           WRITE ( LOUT, '(A)' ) '*'
!
! ======== SOLUTION/CONSTRAINT_EQUATION_VECTOR block
!
           WRITE ( UNIT=LOUT, FMT='(A)' ) '+SOLUTION/CONSTRAINT_EQUATION_VECTOR'
           WRITE ( UNIT=LOUT, FMT='(A)' ) '*Index  Right_hand_value____  '// &
     &                                    'Reciprocal_weight___'
!
           IEQU_COU = 0
           DO 450 J5=1,CNSTROBJ%N_EQUAT
              ITP = LTM_DIF ( 1, N__CNS, NAME__CNS, CNSTROBJ%ABB_CNS(J5) )
              IF ( CNSTROBJ%USR_CNS(J5) ) THEN
!
! ---------------- Do not make units transformation in the case of user
! ---------------- constraints. A user brings full responsibility for using
! ---------------- appropriate units
!
                   IEQU_COU = IEQU_COU + 1
                   WRITE ( UNIT=LOUT, FMT=140 ) IEQU_COU, &
     &                                          CNSTROBJ%RTP_CNS(J5), &
     &                                          DABS(CNSTROBJ%SIG_CNS(J5))
                 ELSE
                   IEQU_COU = IEQU_COU + 1
                   WRITE ( UNIT=LOUT, FMT=140 ) IEQU_COU, &
     &                     CNSTROBJ%RTP_CNS(J5)*SCAL__CNS(ITP), &
     &                     DABS(CNSTROBJ%SIG_CNS(J5)*SCAL__CNS(ITP))
 140              FORMAT ( 1X,I5, 1X,1PD21.14, 1X,1PD21.14 )
              END IF
 450       CONTINUE
!
           WRITE ( LOUT, '(A)' ) '-SOLUTION/CONSTRAINT_EQUATION_VECTOR'
           WRITE ( LOUT, '(A)' ) '*'
           WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                           '---------------------------------------'
           WRITE ( LOUT, '(A)' ) '*'
!
! ======== SOLUTION/CONSTRAINT_WEIGHT_MATRIX block
!
           WRITE ( LOUT, '(A)' ) '+SOLUTION/CONSTRAINT_WEIGHT_MATRIX'
           WRITE ( LOUT, '(A)' ) '* Ind1  Ind2 Weight_matrix(ind1,ind2)'
      END IF
!
      IF ( FL_SINEX_CNS  .OR.  FL_SINEX_DCM ) THEN
!
! -------- Filling elements of WEI_CNS matrix
!
           DO 460 J6=1,CNSTROBJ%N_EQUAT
              IF ( CNSTROBJ%USR_CNS(J6) ) THEN
                   WEI_CNS( LOCC(J6,J6) ) = 1.D0/( CNSTROBJ%SIG_CNS(J6) )**2
                 ELSE
                   ITP = LTM_DIF ( 1, N__CNS, NAME__CNS, CNSTROBJ%ABB_CNS(J6) )
                   IF ( ITP .LE. 0  ) THEN
                        CALL ERR_LOG ( 3773, IUER, 'WRITE_SINEX_CONT', &
     &                      'Trap of internal control: constraint '// &
     &                       CNSTROBJ%ABB_CNS(J6)//' was not found in the '// &
     &                      'list of support constraint abbreviations '// &
     &                      'NAME__CNS defiend in glbc4.i' )
                        RETURN
                   END IF
!
                   WEI_CNS( LOCC(J6,J6) ) = &
     &                    1.D0/( CNSTROBJ%SIG_CNS(J6)*SCAL__CNS(ITP) )**2
              END IF
 460       CONTINUE
!
! -------- Putting off-diagional elements of wight matrix of constraint
!
           DO 470 J7=1,CNSTROBJ%N_OFD
              ITP1 = LTM_DIF ( 1, N__CNS, NAME__CNS, &
     &                            CNSTROBJ%ABB_CNS(CNSTROBJ%INE1_OFD(J7)) )
              ITP2 = LTM_DIF ( 1, N__CNS, NAME__CNS, &
     &                            CNSTROBJ%ABB_CNS(CNSTROBJ%INE2_OFD(J7)) )
              IF ( CNSTROBJ%USR_CNS(J7) ) THEN
                   WEI_CNS ( LOCC(CNSTROBJ%INE1_OFD(J7),CNSTROBJ%INE2_OFD(J7)) ) = &
     &                       CNSTROBJ%WEI_OFD(J7)
                 ELSE
                   WEI_CNS ( LOCC(CNSTROBJ%INE1_OFD(J7),CNSTROBJ%INE2_OFD(J7)) ) = &
     &                  CNSTROBJ%WEI_OFD(J7)/( SCAL__CNS(ITP1)*SCAL__CNS(ITP2) )
              END IF
 470       CONTINUE
!
! -------- Writing down elements of the weight matrix of constraints
!
           DO 480 J8=1,CNSTROBJ%N_EQUAT
              DO 490 J9=1,J8
                 IF ( DABS( WEI_CNS( LOCC(J8,J9) ) ) .GT. SMALL_EPS**2 ) THEN
                      IF ( FL_SINEX_CNS ) THEN
                           WRITE ( UNIT=LOUT, FMT=150 ) J8, J9, &
     &                                                  WEI_CNS(LOCC(J8,J9))
 150                       FORMAT ( 1X,I5, 1X,I5, 1X,1PD21.14 )
                      END IF
                 END IF
 490          CONTINUE
 480       CONTINUE
      END IF
!
      IF ( FL_SINEX_CNS  ) THEN
           WRITE ( LOUT, '(A)' ) '-SOLUTION/CONSTRAINT_WEIGHT_MATRIX'
           WRITE ( LOUT, '(A)' ) '*'
           WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                            '---------------------------------------'
           WRITE ( LOUT, '(A)' ) '*'
      END IF
!
      IF ( FL_SINEX_DCM ) THEN
!
! -------- Scaling partial covariance matrix before inversion
!
           CALL SCALER ( NOR_DCM, NRV_DCM, CNS_WMA, INT2(L_SPR) )
!
! -------- Invert parital convariance matrix
!
           CALL ERR_PASS ( IUER, IER )
           CALL INVS     ( L_SPR, NOR_DCM, RC, IER )
           IF ( IER .NE. 0 ) THEN
                WRITE ( 6, * ) ' L_SPR=',L_SPR
                CALL ERR_LOG ( 3774, IUER, 'WRITE_SINEX_CONT', 'Error in '// &
     &              'an attempt to invert partial covariance matrix' )
                RETURN
           END IF
!
! -------- Unscaling the invert of the partial covariance matrix
!
           CALL UNSCALER ( NOR_DCM, NRV_DCM, CNS_WMA, INT2(L_SPR) )
!
           IF ( CNSTROBJ%N_EQUAT .GT. 0 ) THEN
!
! ------------- Computing NOR_DCM : = NOR_DCM - CNS_MAT(T) * WEI_CNS * CNS_MAT
!
                CALL MUL_MM_SI_I ( CNSTROBJ%N_EQUAT, WEI_CNS, &
     &                             CNSTROBJ%N_EQUAT, L_SPR, CNS_MAT, &
     &                             CNSTROBJ%N_EQUAT, L_SPR, CNS_WMA, IER )
                CALL MUL_MM_TI_S ( CNSTROBJ%N_EQUAT, L_SPR, CNS_MAT, &
     &                             CNSTROBJ%N_EQUAT, L_SPR, CNS_WMA, &
     &                             L_SPR, CNS_NRM, IER )
!!!!!!!!!!!!!!!!!!!
!                  call matview_2 ( l_spr, nor_dcm ) ! %%%
!                  call matview_2 ( l_spr, nor_dcm ) ! %%%
!                  call matview_2 ( cnstrobj%n_equat, wei_cns ) ! %%%
!                  call matview_2 ( l_spr, cns_nrm ) ! %%%
!                  call matview_1 ( cnstrobj.n_equat, l_spr, cns_mat ) ! %%%
!                  call matview_2 ( cnstrobj.n_equat, wei_cns ) ! %%%
!!!!!!!!!!!!!!!!!!!
                CALL SUB_VV ( (L_SPR*(L_SPR+1))/2, NOR_DCM, CNS_NRM )
           END IF
!
! -------- Compute NRV_DCM := NOR_DCM * ADJ_VEC
!
           CALL MUL_MV_SV_V ( L_SPR, NOR_DCM, L_SPR, ADJ_VEC, L_SPR, &
     &                               NRV_DCM, IER )
!
! ======== SOLUTION/DECOMPOSED_NORMAL_MATRIX  block
!
           WRITE ( LOUT, '(A)' ) '+SOLUTION/DECOMPOSED_NORMAL_MATRIX'
           WRITE ( LOUT, '(A)' ) '* Ind1  Ind2 '// &
     &                           'Decomposed_normal_matrix(ind1,ind2)'
!
           DO 4100 J10=1,L_SPR
              DO 4110 J11=1,J10
                 WRITE ( UNIT=LOUT, FMT=110 ) J10, J11, NOR_DCM( LOCC(J10,J11) )
 4110         CONTINUE
 4100      CONTINUE
!
           WRITE ( LOUT, '(A)' ) '-SOLUTION/DECOMPOSED_NORMAL_MATRIX'
           WRITE ( LOUT, '(A)' ) '*'
           WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                            '---------------------------------------'
           WRITE ( LOUT, '(A)' ) '*'
           WRITE ( LOUT, '(A)' ) '+SOLUTION/DECOMPOSED_NORMAL_VECTOR'
           WRITE ( LOUT, '(A)' ) '* Ind1  Decomposed_vector(ind1)'
!
! ======== SOLUTION/DECOMPOSED_NORMAL_VECTOR  block
!
           DO 4120 J12=1,L_SPR
              WRITE ( UNIT=LOUT, FMT=140 ) J12, NRV_DCM(J12)
 4120      CONTINUE
           WRITE ( LOUT, '(A)' ) '-SOLUTION/DECOMPOSED_NORMAL_VECTOR'
           WRITE ( LOUT, '(A)' ) '*'
           WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                            '---------------------------------------'
           WRITE ( LOUT, '(A)' ) '*'
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  WRITE_SINEX_CONT  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   CHAR26 ( IVAL )
! ************************************************************************
! *                                                                      *
! *   Routine CHAR26 transforms the value IVAL to the 2-characters line  *
! *   string in module 26.                                               *
! *                                                                      *
! *  ### 02-OCT-2002     CHAR26    v1.0 (c)  L. Petrov  02-OCT-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      CHARACTER  CHAR26*2
      INTEGER*4  IVAL
      INTEGER*4  ID1, ID2
!
      ID1 = IVAL/26
      IF ( ID1 .GT. 0 .AND. ( IVAL - ID1*26 ) .EQ. 0 ) ID1 = ID1-1
      ID2 = IVAL - 26*ID1
      IF ( ID1 .EQ. 0 ) THEN
           CHAR26(1:1) = ' '
         ELSE
           CHAR26(1:1) = CHAR(64+ID1)
      END IF
!
      IF ( ID2 .EQ. 0 ) THEN
           CHAR26(2:2) = 'A'
         ELSE
           CHAR26(2:2) = CHAR(64+ID2+1)
      END IF
!
      RETURN
      END  !#!  CHAR26  #!#
