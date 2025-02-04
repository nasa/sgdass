      SUBROUTINE ARCSET ( DONE, ARCNUM, IPASS, SOLTY2, CGMNMR, B_ARCDIR, &
     &           ID, USER_PROG, B_KPERMARC, WEIGHTS, LF_WEI, WEIGHT_FILE, &
     &           STAFLG, FIXSTA_CHR, STADIU, VELFLG, SRCFLG, FIXSRC_CHR, &
     &           PROFLG, NUTFLG, UT1FLG, PRCFLG, RELFLG, IONCTL, &
     &           MINOUT, BASOUT, FWDOUT, SCNOUT, STACRY, &
     &           DBNAME, VER, KLAST, KFAIL, KCORL, IDBNAME, &
     &           QICOV, QJCOV, ATMFLG, INTRVL, FCNPR, &
     &           QATMCNST, CLKPOL_FLG, CLKPOL_DEG, CLKFLG, CKNTRVL, &
     &           QCLKCNST, ITARCS, SOLTYO, &
     &           QCLKEXCPT, QATMEXCPT, TBLOUT, AXSFLG, CLKCNS, &
     &           ATMCNS, EOPSIG, EOPCNS, OFFLG, RATFLG, ACCEOP_FLG, &
     &           IEOP_FLG, REOP_FLG, IEOPLL, NESM, ESMSITES, ESMDATES, &
     &           EOPRSIG, EOPRCNS, EOPFACT, POSELL, REFREQ, PWCCNS, &
     &           QPWCCNST, BLCFLG, IOS_EST_BATCH, IOS_SIG_BATCH, NEXCBL, IBLNM, &
     &           BASDF, EOPMID, IONFLG, NO_SUPERFILE, USER_BUFF, SOLTYP, RESFILE, &
     &           KMIN_SIG, NUTSIG, NUTCNS, GRADCNS, QGRADCNST, &
     &           GRADFLG, GRINTRVL, KOUTNRM, KZERONRM, KSTACONST, KVELCONST, &
     &           DBNAME_MES, WEIGHT_TYPE_GEN, L_WEI, &
     &           SUPNAM_WEI, SUPVER_WEI, BASELINE_WEI, ARR_WEI, &
     &           EOP_EPOCH_MJD, EOP_EPOCH_SEC, EOP_BEFORE_SEC_TAI, EOP_AFTER_SEC_TAI, &
     &           FL_GVF, GVH, PARU_FILE_ARC, WEIGHT_ALGORITHM, VCAT )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ARCSET PROGRAM SPECIFICATION
!
! 1.1 Set up solution for each arc.
!
! 1.2 REFERENCES:
!
! 2.  ARCSET INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) SOLTY2, STAFLG, VELFLG, SRCFLG, PROFLG, NUTFLG(116), &
     &              UT1FLG, MINOUT, FIXSTA_CHR, FIXSRC_CHR
      CHARACTER*(*) STACRY, ATMFLG, CLKPOL_FLG, CLKFLG,USER_PROG, &
     &              WEIGHTS, WEIGHT_FILE(*)*(*)
      CHARACTER*(*) BASOUT, SCNOUT, PRCFLG, RELFLG, CGMNMR, ID
      CHARACTER*(*) IONCTL, STADIU, IDBNAME, QICOV, QJCOV, SOLTYO
      CHARACTER*(*) TBLOUT, AXSFLG, CLKCNS, ATMCNS, EOPCNS, POSELL
      CHARACTER*(*) B_ARCDIR(3), EOPRCNS, ESMSITES(MAX_ESM), REFREQ
      CHARACTER*(*) PWCCNS, BLCFLG, BASDF, IONFLG, SOLTYP, RESFILE, NUTCNS
      CHARACTER*(*) GRADCNS, GRADFLG, DBNAME, DBNAME_MES, &
     &              WEIGHT_TYPE_GEN, PARU_FILE_ARC*(*)
      CHARACTER*128 VTD_CONF*128, STR_EQUAL_EFF_FREQ*4
      INTEGER*4     WEIGHT_ALGORITHM
      INTEGER*2     IPASS, NESM, NEXCBL, IBLNM(8,50), &
     &              FWDOUT,ITARCS,INTRVL, CLKPOL_DEG, CKNTRVL, &
     &              OFFLG, RATFLG, GRINTRVL, ACCEOP_FLG, &
     &              QCLKEXCPT(4), QATMEXCPT(4), IEOP_FLG(6), IEOPLL, EOPMID, &
     &              SUPVER_WEI(MAX4_WEIREC), ierr
      REAL*8        REOP_FLG(4), ESMDATES(MAX_ESM), EOPFACT, EOP_BEFORE_SEC_TAI, &
     &              EOP_AFTER_SEC_TAI
      LOGICAL*2     KCORL, B_KPERMARC, KMIN_SIG, KOUTNRM, KZERONRM
      REAL*8        FCNPR, QATMCNST(2), QCLKCNST(2), EOPSIG(3), EOPRSIG(3), &
     &              QPWCCNST, NUTSIG(2), QGRADCNST(2)
      LOGICAL*2     KSTACONST, KVELCONST
      CHARACTER     DATYP__ABR__ARR(FIRST__DTP:LAST__DTP)*6
      INTEGER*4     LF_WEI, L_WEI, EOP_EPOCH_MJD, EOP_EPOCH_MJD_SAVE
      INTEGER*2     EOPMID_SAVE
      REAL*8        ARR_WEI(4,MAX4_WEIREC), EOP_EPOCH_SEC, EOP_EPOCH_SEC_SAVE, &
     &              DTEC_ERR_SCL_VAR
      CHARACTER     SUPNAM_WEI(MAX4_WEIREC)*10, BASELINE_WEI(MAX4_WEIREC)*16
      LOGICAL*4     FL_NOCAL  ! Do not use any calibrations
      LOGICAL*4     FL_NOCONT ! Do not use any contributions
      LOGICAL*4     FL_NOMAP  ! Do not use any mappings
      LOGICAL*4     FL_DTEC_SBA_USE
!
! ATMCNS - Atmosphere constraint flag
! ATMFLG - atmosphere flag
! AXSFLG - axis offset flag
! B_ARCDIR - directories in which to write arc files
! B_KPERMARC - TRUE if arc files to be saved permanently
! BASOUT - Baseline output flag
! CGMNMR - name of input CGM file
! CLKCNS - Clock constraint flag
! CLKFLG - clock flag
! CKNTRVL - clock interval, minutes
! EOPCNS - Earth orientation constraint flag
! EOPSIG - Earth orientation constraint values
! FCNPR - free core nutation period
! FIXSRC_CHR - reference source name
! FIXSTA_CHR - reference station name
! FWDOUT - flag for output for forward solution
! INTRVL - atmosphere interval, minutes
! ID - String identifying this batch run
! IDBNAME - Database name for which to output covariances
! IONCTL - ionosphere control flag
! IPASS - Pass number; 1 = forward, 2 = back
! ITARCS - number of arcs in control file
! KCORL - TRUE if covariances are to be output
! MINOUT - flag for minimum output to spool file
! VELFLG - velocities flag
! NUTFLG - nutation flag
! PRCFLG - precession flag
! QATMCNST - atmosphere constraints
! QATMEXCPT - Exception station for atmosphere constraints
! QCLKCNST - Clock constraints
! QCLKEXCPT - Exception station for clock constraints
! QICOV - Type of covariance to output
! RELFLG - relativity flag
! SCNOUT - screen output flag
! SOLTYO - solution type (Complete, Back, etc.)
! SOLTY2 - Solution type for the current pass (Forward or Back)
! SRCFLG - source positions flag
! SRCFLG - source propser motioin flag
! STADIU - station diurnal flag
! STACRY - station carry flag
! STAFLG - station flag
! TBLOUT - station table flag
! USER_PROG - User program
! UT1FLG - earth orientation flag
! WEIGHTS - weights flag
! LF_WEI         - the number of weight files
! WEIGHT_FILE - Name of files containing weights
! offlg - Earth orientation offset flag
! ratflg - Earth orientation rate flag
! ACCEOP_FLG - Earth orientation acceleration flag.
! ieop_flg - Local earth orientation flags
! ieopll - Earth orientation plot flag
! reop_flg - Earth orientation intervals and constraints
! WEIGHT_TYPE_GEN -- weigths type: A -- session, B -- baseline, S -- site
! L_WEI - number of elements in weights arrays
! SUPNAM_WEI - Array of superfile names for wight array
! SUPVER_WEI -- Array of superfile versions for weight array
! BASELINE_WEI -- Array of baseline names for weight array
! ARR_WEI -- Array of baseline (or station ) weights which corresponds to
!            weights file records
! WEIGHT_ALGORITHM -- algorithm for computing weights
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2 DONE, KLAST, KFAIL, NO_SUPERFILE
      INTEGER*2 VER,ARCNUM,fixnam(4)
      CHARACTER*(*) USER_BUFF
      LOGICAL*2 DO_CLK_REF   ! Do or don't specify ref clock in arc line.
      INTEGER*4 NUM_CLK_REF  ! Number of clock reference sites
      CHARACTER*8 LIST_CLK_REF(MAX_ARC_STA) ! List of 8-character names
!                                           ! of reference sites
      LOGICAL*2 DO_STA_EXC          ! Do or don't exclude station(s)
      INTEGER*4 NUM_STA_EXC         ! number of stations to be excluded
      CHARACTER*8 LIST_STA_EXC(MAX_ARC_STA) ! List of 8-character names of
!                                           ! stations to be excluded
!
! ARCNUM - Number of current arc
! DBNAME - Name of database for current arc
! DBNAME_MES - Name of database + version number for current arc
! DONE - TRUE when last arc has been done
! KFAIL - TRUE if this arc fails
! KLAST - TRUE if this is the last arc
! VER - superfile version number of current arc
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'precm.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'bdata.i'
      INCLUDE 'batme.i'
      INCLUDE 'calcm.i'
      INCLUDE 'dmapp.i'
      INCLUDE 'fbcom.i'
      INCLUDE 'gvh.i'
      INCLUDE 'vcat.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: prces
!       CALLED SUBROUTINES: domapp,cfpos,set_mask,glsta,lcsta,sflags,
!                           garc,ddata,cfread,delev,cfunrd,setnam
!
! 3.  LOCAL VARIABLES
!
      TYPE ( GVH__STRU     ) ::  GVH
      TYPE ( VCAT__TYPE    ) ::  VCAT
      CHARACTER   ORIENT*1, EOPCOK*1, EOPMOD*1, INTTODECIMAL*2
      CHARACTER   ROTFLG*3, MODETYP*6, LDTYPE*6
      CHARACTER   NAME*16, STRING*8192, EOP_SUPR*6, BASDEP_CLO*3, CISITN*8
      INTEGER*2   SUPMET_ARC, IDUM, INAME(8),IBATCH, IISITN(4)
      EQUIVALENCE (CISITN,IISITN(1))
      INTEGER*2   LENGTH,NRTARC,ISTAD(4),dbn(5)
      INTEGER*2   DECIMALTOINT, CFREAD, I1, I2, ILCSTA, IGLSTA
      LOGICAL*2   CFEOF, EQUAL, KBIT
      LOGICAL*2   FIND_WEIGHTS, KPWC, LFLAG_FIND, OLD_LOGWEI
      CHARACTER   ORIENT_OLD*1, STR*80
      INTEGER*2   I, J, MODCONTROL, IOS_EST_BATCH, TRIMLEN
      REAL*8      CURJUL, FIRST_JDATE, LAST_JDATE
      REAL*8      CONSTANTS(4,MAX_ARC_BSL)
      CHARACTER   CLKCNS_USE*1, ATMCNS_USE*1, GRADCNS_USE*1, EDIT_FILE*128, &
     &            AOC_FILE*128, ADDW_FILE*128, DTEC_FILE*128, EXT_ERR_FILE*128
      REAL*8      CLOCK_CNS_SIG, ATM_CNS_SIG, GRAD_CNS_OFFS_SIG, &
     &            GRAD_CNS_RATE_SIG, EOPSIG_ARC(3), EOPRSIG_ARC(3), &
     &            IOS_SIG_BATCH
      CHARACTER*1 WEIGHT_TYPE_UR
      LOGICAL*4   FL_GVF, FL_EOP_CONS, FL_EOPR_CONS
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4   IUER
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      EQUIVALENCE (INAME(1),NAME)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH  901128  Enabled new earth orientation parameterization scheme
!   aee  910419  Added srccmp for proper motions
!   mwh  910813  Implement eop rate constraints
!   jmg  960610  Fix bug which occurs because
!                the fixed arc is treated differently than the free arcs.
!                Previously the EOP parameters were not estimated in the
!                forward solution -- only in the back.  Now they will be
!                estimated in both directions.  This requires the application
!                of constraints in arcpe to constrain the adjustments to zero.
!   kdb  970204  New site weighting feature.
!   PET  971006  SOCOM_EXT and FAST_BYPASS added before PARCN in order to change
!                FAST mode if it is necssary and in order to provide correct way
!                for calculation the number of estimated parameters
!   jwr  971020  Logic to handle clock reference sites in the batch control file
!                arc line.
!   pet  971204  Added logic for splitting baseline deselection on phase delay
!                and group delay solution type
!   pet  971211  Fixed  DATABASE_DATE-NOV97 bug: put call of  OBSTM  instead
!                of KEYJL. OBSTM returns julian date at the middle of the
!                session, KEYJL parsed the database name and abended when
!                database name was not in $YYMMMDDEE fromat.
!   pet  980116  Fixed bug: parfil should be written before call socom_ext.
!   pet  980203  Substituted hard-coded test of solution types by named
!                constants from ../include/solve.i
!   kdb  980223  Batch interface for sinex output
!   pet  980401  Added support of extended list of solution type
!                abbreviations.
!   pet  980708  Changed logic to support the situation when the first arc(s)
!                was skipped by PROC and ARCPE has not created output CGM
!   pet  980731  Added a formal parameters EOP_SUPR for GARC and SFLAGS
!                (it allows to deselect certain EOP component form solution
!                 for the certain arc)
!   pet  990104  Added a formal parameters BASDEP_CLO for GARC and SFLAGS
!                (it allows to specify estimation of baseline-dependent
!                 clocks for the certain arc)
!   pet  990407  Improved error message about failure to find used weights.
!                If weights are used (but not required) then an error message
!                will be printed only if G_WARNING is true.
!   pet  990507  Changed logic of setting INAMCG in NO TRAIN mode
!   pet  1999.10.15  Added support of station session-dependent de-selection:
!                    a) added three new parameters in the GARC arguments list;
!                    b) add two new parameters in DDATA arguments list
!   pet  2000.03.29  Added support of "by_baseline" weights type
!   pet  2000.05.11  Added support of WEIGHTS 'N' feature.
!   pet  2000.10.05  Corrected a bug: EOPMID should be of INTEGER*2 type!
!   pet  2000.11.13  Added support the option IN_EOP_CONSTRAINT of the arc-line.
!                    Then this opotion is ineffect EOP constraints are set in
!                    according to the covaraince matrix in the eop modfile.
!   pet  2000.11.21  Moved call of DISCONT_INIT to ctrls since some data
!                    structures should be initialized before the
!                    first call of arcset in the mode with input CGM.
!   pet  2001.05.31  Changed the logic so that it reads obsfil when it processes
!                    the experiment in "make weights" mode
!   pet  2001.08.10  converted type of FIXSTA, FIXSRC ( INTEGER*2 ) to
!                    FIXSTA_CHR, FIXSRC_CHR ( CHARACTER ). Removed STACMP,
!                    SRCCMP, TIDFLG. Added VELFLG, PROFLG
!   pet  2001.09.05  Added new formal parameters: WEIGHT_TYPE_GEN, L_WEI,
!                    SUPNAM_WEI, SUPVER_WEI, BASELINE_WEI, ARR_WEI
!                    Changing logic of applying weights. The old logic required
!                    reading weights file. The new logic read the weihgts file
!                    in prces only once. Weights arrays are passed to arcset.
!                    New subroutine APPLY_WEIGHTS seeks weights in these
!                    arrays instead of file
!   pet  2001.12.13  Added support of a new type of solutions: GLOBAL_ONLY
!                    ( SOLTYP = 'G' )
!   pet  2002.03.18  Removed unnecessary argument KFAIL from call to ddata
!   pet  2002.03.27  Remved arguments LSINEX, LLSNXDIR
!   pet  2002.04.01  Added printing name of user program, user buffer and
!                    user partial program at the listing header.
!   pet  2002.05.30  Fixed a bug: the previous version used incorrect logic
!                    for setting "make baseline table" flag: it did not set
!                    this flag in independent solutions when "STATIONS NO"
!                    was specified in the $CARYY section. The new version
!                    sets "make baseline table" flag in independent solution
!                    if BASELINBES YES and position of at least one station
!                    are being estimated regardless of what user has specifed
!                    in $CARRY sectin (which is meaningless in independent
!                    solution mode)
!   pet  2002.10.04  Added initialization of variable GLO_FJDOBS_BEG,
!                    GLO_FJDOBS_END if it is the first session of the forward
!                    run.
!   jwr  2003.05.15  Sleeping bug in call to decimaltoint fixed.
!
!   pet  2004.03.16  Revised logic of setting constraints in order to support
!                    an ability to set constaitns for this session only
!                    according to options in a session line
!
!   pet  2004.03.18  Fixed a bug: the previous version incorrectly handled
!                    situation when atmosphere constraint usage eas "MOST",
!                    but no constaints were applied
!
!   pet   2004.03.19 Fixed a typo in the logic for dealing with setting sigmas
!                    of constraints on clocks and atmosphere
!
!   pet   2004.12.02 Recent changs awoke a sleeping bug: EOP constraints bit
!                    logic was wrong: if EOPMOD = 'N', then EOP constraint
!                    bit should be lifted
!
!   pet   2005.09.04 Added support of SUPMET_ARC variable
!
!   pet  2007.09.24  Added support of keyword EOPR_CONSTRAINT
!
!   pet  2019.05.09  Prevented a crash in an attempt of operation mod(k,0) 
!
!   pet  2020.04.27  Added support of the edit file
!
!   pet  2020.07.14  Added support of the additive weight file
!
!   pet  31-DEC-2021 Added support of the dTEC external file
!
!   pet  2022.08.22  Added support of the IOS_EST_BATCH and IOS_SIG_BATCH variables &
!                    for setting estimation and constrain of ionospheric path delay
!                    scale.
!
! 5.  ARCSET PROGRAM STRUCTURE
!
!
! --- Transforming DATYP__ABR from one condensed string to an array of
! --- 6-symbols strings
!
      CALL LIB$MOVC3 ( 6*(LAST__DTP-FIRST__DTP+1), DATYP__ABR, &
     &                 DATYP__ABR__ARR )
!
! --- Get the scratch files
!
      PRE_IP(4) = -1
      EOPMID_SAVE = EOPMID
      EOP_EPOCH_SEC_SAVE = EOP_EPOCH_SEC
      EOP_EPOCH_MJD_SAVE = EOP_EPOCH_MJD
!
      CALL GETENVAR ( 'EQUAL_EFF_FREQ', STR_EQUAL_EFF_FREQ )
      IF ( STR_EQUAL_EFF_FREQ(1:3) == 'YES' .OR. STR_EQUAL_EFF_FREQ(1:3) == 'yes' ) THEN
           FL_EQUAL_EFF_FREQ = .TRUE.
         ELSE
           FL_EQUAL_EFF_FREQ = .FALSE.
      END IF
      DO WHILE ( PRE_IP(4) .EQ. -1 )
!
! ------- Parse the next line of the arc-file
!
          CALL GARC ( DONE, ORIENT, DBNAME, VER, SCNOUT, ROTFLG, NRTARC, &
     &                MODETYP, SUPMET_ARC, USER_PROG, EOPCOK, EOPMOD, SOLTY2, &
     &                MODCONTROL, NO_SUPERFILE, USER_BUFF, DO_CLK_REF, &
     &                NUM_CLK_REF, LIST_CLK_REF, EOP_SUPR, BASDEP_CLO, &
     &                WEIGHTS, EOPSIG_ARC, EOPRSIG_ARC, FL_NOCAL, FL_NOCONT, &
     &                FL_NOMAP, DBNAME_MES, EOPMID, EOP_EPOCH_MJD, &
     &                EOP_EPOCH_SEC, GVH, PARU_FILE_ARC, WEIGHT_ALGORITHM, &
     &                FL_EOP_CONS, FL_EOPR_CONS, EDIT_FILE, AOC_FILE, &
     &                ADDW_FILE, DTEC_FILE, ADDW_SCL, FL_DTEC_SBA_USE, &
     &                DTEC_ERR_SCL_VAR, EXT_ERR_FILE, VCAT )
          IF ( DONE ) RETURN
       ENDDO
!
! --- Open and read common block off disk
!
      CALL USE_COMMON ( 'ORC' )
!
! --- Open and read parfil
!
      CALL USE_PARFIL   ( 'ORC' )
      CALL USE_GLBFIL_4 ( 'ORC' )
!
! --- Copy the edit file
!
      EDIT_FIL = EDIT_FILE
      AOC_FIL  = AOC_FILE
      ADDW_FIL = ADDW_FILE
      DTEC_FIL = DTEC_FILE
      DTEC_SBA_USE = FL_DTEC_SBA_USE
      DTEC_ERR_SCL = DTEC_ERR_SCL_VAR 
      EXT_ERR_FIL  = EXT_ERR_FILE
!
! --- Add station parameters for piecewise continuous linear stations
!
      CALL NOUT_R8 ( INT4(MAX_STA), PSITED ) ! Initialization
!
      DO I=1,NUMSTA
         KPWC = .FALSE.
         DO J=1,PWCNUM(2)
            CISITN(1:8) = PWCSITES(J)
            IF ( EQUAL( ISITN(1,I), INT2(1), IISITN, INT2(1), INT2(8) ) ) THEN
                 KPWC = .TRUE.
                 PSITED(I) = PWCEP(1)
            ENDIF
         ENDDO
!
         IF ( KPWC ) THEN
              PSITED(I) = PWCEP(1)
         ENDIF
      ENDDO
!
      PWCNUMEP = PWCNUM(1)
      PWCSIZEP = PWC_INTRVL
      CALL USE_COMMON ( 'OWC' )
      CALL USE_PARFIL ( 'OWC' )
!
      LENGTH = CFREAD(STRING)
      KLAST = STRING(1:1) .EQ. '$'  .OR. CFEOF(IDUM)
      CALL CFUNRD ( LENGTH, STRING )
!
      ARCNUM = ARCNUM + 1
!
! --- De-select any data that were not wanted
!
      CALL DDATA ( ISTAD, NUM_STAINC, LIST_STAINC, &
     &                    NUM_STAEXC, LIST_STAEXC, NUM_SOUEXC, LIST_SOUEXC )
!
! --- Set elvcut array
!
      CALL DELEV()
!
! --- Set constraint_bits flags and constraints
! --- If constraints are there and the choice was MOST, then
! --- pick the weakest of the existing and the request
!
      ATMCNS_USE = ATMCNS
      IF ( ATM_CNS_TYP .NE. ' '    ) ATMCNS_USE  = ATM_CNS_TYP
      ATM_CNS_SIG = QATMCNST(1)
      IF ( ATM_CNS_VAL .GT. 1.D-15 ) ATM_CNS_SIG = ATM_CNS_VAL
!
!!      IF ( ATMCNS_USE .EQ. 'M'      .AND.  &
!!     &     DBNAME_CH(1:1) .NE. '$'         ) THEN
!!           CALL ERR_LOG ( 4871, IUER, 'ARCSET', 'Attempt to use '// &
!!     &         'ATMCNS MOST in the $CONSTRAINTS section for the database '// &
!!     &          DBNAME_CH//' which has no atmosphere constraints '// &
!!     &         'parameterization' )
!!           CALL EXIT ( 1 )
!!           RETURN 
!!      END IF
!
      IF ( ATMCNS_USE .EQ. 'M' ) THEN
           IF ( KBIT ( CONSTRAINT_BITS, INT2(2) ) ) THEN
                IF ( SITE_DEP_CONST ) THEN
                     DO I=1,NUMSTA
                        SACNST(I) = MAX ( ATM_CNS_SIG, SACNST(I) )
                     ENDDO
                  ELSE
!
! ------------------ If not site dependent use SACNST(1) in only,
! ------------------ in case others aren't set
!
                     DO I=1,NUMSTA
                        SACNST(I) = MAX ( ATM_CNS_SIG, SACNST(1) )
                     ENDDO
                ENDIF
              ELSE
!
! ------------- There are no existing constraints, so use the specified value
!
                DO I=1,NUMSTA
                   SACNST(I) = ATM_CNS_SIG
                ENDDO
            END IF
          ELSE IF ( ATMCNS_USE .EQ. 'N' ) THEN
!
! --------- Disable constraints
!
            DO I=1,NUMSTA
               SACNST(I) = 0.0D0
            ENDDO
          ELSE ! 'A', 'Y'
!
! --------- There are no existing constraints, so use the specified value
!
            DO I=1,NUMSTA
               SACNST(I) = ATM_CNS_SIG
            ENDDO
      ENDIF
!
! --- Now do the same thing for clocks for 'most'
!
      CLKCNS_USE = CLKCNS
      IF ( CLOCK_CNS_TYP .NE. ' ' ) CLKCNS_USE = CLOCK_CNS_TYP
      CLOCK_CNS_SIG = QCLKCNST(1)
      IF ( CLOCK_CNS_VAL .GT. 1.D-15 ) CLOCK_CNS_SIG = CLOCK_CNS_VAL
!
!!      IF ( CLKCNS_USE .EQ. 'M'  .AND. &
!!     &     DBNAME_CH(1:1) .NE. '$'       ) THEN
!!           CALL ERR_LOG ( 4872, IUER, 'ARCSET', 'Attempt to use '// &
!!     &         'CLOCKS MOST in the $CONSTRAINTS section for the database '// &
!!     &          DBNAME_CH//' which has no clock constraints parameterization' )
!!           CALL EXIT ( 1 )
!!           RETURN 
!!      END IF
!
      IF ( CLKCNS_USE .EQ. 'M' ) THEN
           IF ( KBIT ( CONSTRAINT_BITS, INT2(3) ) ) THEN
                IF ( SITE_DEP_CONST ) THEN
                     DO I=1,NUMSTA
                        SCCNST(I) = MAX ( CLOCK_CNS_SIG, SCCNST(I) )
                     ENDDO
                  ELSE
!
! ------------------ If not site dependent use SCCNST(1) in only,
! ------------------ in case others aren't set
!
                     DO I=1,NUMSTA
                        SCCNST(I) = MAX ( CLOCK_CNS_SIG, SCCNST(1) )
                     ENDDO
                ENDIF
             ELSE
!
! ------------- There are no existing constraints, so use the specified value
!
                DO I=1,NUMSTA
                   SCCNST(I) = CLOCK_CNS_SIG
                ENDDO
           ENDIF
         ELSE IF ( CLKCNS_USE .EQ. 'I' ) THEN
           CONTINUE 
         ELSE IF ( CLKCNS_USE .EQ. 'N' ) THEN
!
! -------- No constraints
!
           DO I=1,NUMSTA
              SCCNST(I) = 0.0D0
           ENDDO
         ELSE ! 'A', 'Y'
!
! -------- There are no existing constraints, so use the specified value
!
           DO I=1,NUMSTA
              SCCNST(I) = CLOCK_CNS_VAL
           ENDDO
      ENDIF
!
! --- Set gradient constraint levels
!
      GRADCNS_USE = GRADCNS
      GRAD_CNS_OFFS_SIG = QGRADCNST(1)
      IF ( GRAD_CNS_OFFS .GT. 1.D-15 ) GRAD_CNS_OFFS_SIG = GRAD_CNS_OFFS
      GRAD_CNS_RATE_SIG = QGRADCNST(2)
      IF ( GRAD_CNS_RATE .GT. 1.D-15 ) GRAD_CNS_RATE_SIG = GRAD_CNS_RATE
!
      IF ( GRAD_CNS_TYP .NE. ' ' ) GRADCNS_USE = GRAD_CNS_TYP
      IF ( GRADCNS_USE .EQ. 'Y'  .OR.  GRADCNS_USE .EQ. 'A' ) THEN
           GRADCONS(1) = GRAD_CNS_OFFS_SIG
           GRADCONS(2) = GRAD_CNS_RATE_SIG
         ELSE
           GRADCONS(1) = 0.0D0
           GRADCONS(2) = 0.0D0
      END IF
!
! --- Set the constraint bits
!
      IF ( EOPCNS  .EQ. 'Y' ) CALL KSBIT ( CONSTRAINT_BITS, INT2(1), TRUE__L2  )
      IF ( EOPCNS  .EQ. 'I' ) CALL KSBIT ( CONSTRAINT_BITS, INT2(1), TRUE__L2  )
      IF ( EOPMOD  .EQ. 'N' ) CALL KSBIT ( CONSTRAINT_BITS, INT2(1), FALSE__L2 )
      IF ( EOPRCNS .EQ. 'Y' ) CALL KSBIT ( CONSTRAINT_BITS, INT2(6), TRUE__L2  )
      IF ( EOPRCNS .EQ. 'N' ) CALL KSBIT ( CONSTRAINT_BITS, INT2(6), FALSE__L2  )
      IF ( EOPMOD  .EQ. 'N' ) CALL KSBIT ( CONSTRAINT_BITS, INT2(6), FALSE__L2 )
      IF ( NUTCNS  .EQ. 'Y' ) CALL KSBIT ( CONSTRAINT_BITS, INT2(7), TRUE__L2  )
!
      IF ( GRADCNS_USE .EQ. 'Y' ) CALL KSBIT ( CONSTRAINT_BITS, INT2(8), TRUE__L2 )
      IF ( GRADCNS_USE .EQ. 'A' ) CALL KSBIT ( CONSTRAINT_BITS, INT2(8), TRUE__L2 )
      IF ( GRADCNS_USE .EQ. 'N' ) CALL KSBIT ( CONSTRAINT_BITS, INT2(8), FALSE__L2 )
!
      IF ( ATMCNS_USE .EQ. 'Y' ) CALL KSBIT ( CONSTRAINT_BITS, INT2(2), TRUE__L2)
      IF ( ATMCNS_USE .EQ. 'A' ) CALL KSBIT ( CONSTRAINT_BITS, INT2(2), TRUE__L2)
      IF ( ATMCNS_USE .EQ. 'M' ) CALL KSBIT ( CONSTRAINT_BITS, INT2(2), TRUE__L2)
      IF ( ATMCNS_USE .EQ. 'N' ) CALL KSBIT ( CONSTRAINT_BITS, INT2(2), FALSE__L2)
!
      IF ( CLKCNS_USE .EQ. 'Y' ) CALL KSBIT ( CONSTRAINT_BITS, INT2(3), TRUE__L2)
      IF ( CLKCNS_USE .EQ. 'A' ) CALL KSBIT ( CONSTRAINT_BITS, INT2(3), TRUE__L2)
      IF ( CLKCNS_USE .EQ. 'M' ) CALL KSBIT ( CONSTRAINT_BITS, INT2(3), TRUE__L2)
      IF ( CLKCNS_USE .EQ. 'N' ) CALL KSBIT ( CONSTRAINT_BITS, INT2(3), FALSE__L2)
!
      IF ( ORIENT .EQ. 'N'  .OR.  EOPCOK .EQ. 'N' ) THEN
           CALL SBIT ( CONSTRAINT_BITS, INT2(1), INT2(0) )
           CALL SBIT ( CONSTRAINT_BITS, INT2(6), INT2(0) )
      ENDIF
!
      IF ( ORIENT .EQ. 'N' ) THEN
           CALL SBIT ( CONSTRAINT_BITS, INT2(7), INT2(0) )
      ENDIF
!
! --- Set EOP RSS sigma values if necessary
!
      IF ( EOPCOK .EQ. 'Y' ) THEN
           DO I=1,3
              IF ( FL_EOP_CONS ) THEN 
                   EOPCONS(I) = EOPSIG_ARC(I)
                 ELSE
                   EOPCONS(I) = EOPSIG(I)
              END IF
              IF ( FL_EOPR_CONS ) THEN 
                   EOPRCONS(I)= EOPRSIG_ARC(I)
                 ELSE 
                   EOPRCONS(I)= EOPRSIG(I)
              END IF
           ENDDO
         ELSE IF ( EOPCOK .EQ. 'N' ) THEN
!
! -------- It was not necessary for this session
!
           DO I=1,3
              EOPCONS(I) = 0.0D0
              EOPRCONS(I)= 0.0D0
           ENDDO
         ELSE IF ( EOPCOK .EQ. 'I' ) THEN
!
! -------- It was not necessary for this session
!
           DO I=1,3
              EOPCONS(I) = 0.0D0
           ENDDO
           CALL SBIT ( CONSTRAINT_BITS, INT2(1), INT2(1) )
           CALL SBIT ( CONSTRAINT_BITS, INT2(6), INT2(1) )
      END IF
!
! --- Set nutation RSS sigma values
!
      DO I=1,2
         NUTCONS(I)=NUTSIG(I)
      ENDDO
!
! --- Set the constraint levels for 'YES'
!
      IF ( ATMCNS_USE .EQ. 'Y' ) THEN
           DO I=1,NUMSTA
              SACNST(I)=QATMCNST(1)
              IF ( EQUAL ( QATMEXCPT, INT2(1), ISITN(1,I), INT2(1), &
     &                    INT2(8) ) ) THEN
                   SACNST(I)=QATMCNST(2)
              ENDIF
           ENDDO
      ENDIF
!
      IF ( CLKCNS_USE .EQ. 'Y' ) THEN
           DO I=1,NUMSTA
              SCCNST(I)=QCLKCNST(1)
              IF ( EQUAL ( QCLKEXCPT, INT2(1), ISITN(1,I), INT2(1), &
     &                     INT2(8)) )THEN
                   SCCNST(I)=QCLKCNST(2)
              ENDIF
           ENDDO
      ENDIF
!
      PWCCNST = 0.0
      IF ( PWCCNS .EQ. 'Y' ) THEN
           PWCCNST = QPWCCNST
      ENDIF
!
! --- Set the flags
!
      CALL OPENNAMFIL()
!
! --- This is a temporary logic for a kludge environment variable
! --- 2001.09.05
!
      CALL GETENVAR ( 'OLD_LOGWEI', STR )
      IF ( STR .EQ. 'YES' .OR. STR .EQ. 'yes' ) THEN
           OLD_LOGWEI = .TRUE.
        ELSE
           OLD_LOGWEI = .FALSE.
      END IF
      IF ( WEIGHTS .EQ. 'U'  .OR.  WEIGHTS .EQ. 'R' ) THEN
!
! >>>>>>>> Beginning of the obsolete logic
!
           IF ( OLD_LOGWEI ) THEN
             LFLAG_FIND = FIND_WEIGHTS ( DBNAME, VER, ISITN, NUMSTA, &
     &                                   LF_WEI, WEIGHT_FILE, WEIGHTS, &
     &                                   WEIGHT_TYPE_UR, CONSTANTS )
             IF ( LFLAG_FIND ) THEN
                IF ( WEIGHT_TYPE_UR .EQ. 'A' .OR. WEIGHT_TYPE_UR .EQ. 'S' ) THEN
                     CALL PUT_WEIGHTS ( CONSTANTS, WEIGHT_TYPE_UR )
                END IF
              ELSE IF ( WEIGHTS .EQ. 'R' ) THEN
                IF ( WEIGHT_TYPE_UR .EQ. 'A' ) THEN
                     CALL FERR ( INT2(317), &
     &                   'BATCH(arcset) Required by_arc weights '// &
     &                   'for session '//DBNAME_MES//' not found in files '// &
     &                    WEIGHT_FILE(1)(1:I_LEN(WEIGHT_FILE(1)))//' '// &
     &                    WEIGHT_FILE(2)(1:I_LEN(WEIGHT_FILE(2)))//' '// &
     &                    WEIGHT_FILE(3)(1:I_LEN(WEIGHT_FILE(3)))//' '// &
     &                    WEIGHT_FILE(4)(1:I_LEN(WEIGHT_FILE(4)))//' ',  &
     &                    INT2(0), INT2(0) )
                   ELSE IF ( WEIGHT_TYPE_UR .EQ. 'S' ) THEN
                     CALL FERR ( INT2(318), &
     &                   'BATCH(arcset) Required site weights '// &
     &                   ' for session '//DBNAME_MES//' not found in files '// &
     &                    WEIGHT_FILE(1)(1:I_LEN(WEIGHT_FILE(1)))//' '// &
     &                    WEIGHT_FILE(2)(1:I_LEN(WEIGHT_FILE(2)))//' '// &
     &                    WEIGHT_FILE(3)(1:I_LEN(WEIGHT_FILE(3)))//' '// &
     &                    WEIGHT_FILE(4)(1:I_LEN(WEIGHT_FILE(4)))//' ',  &
     &                    INT2(0), INT2(0) )
                   ELSE IF ( WEIGHT_TYPE_UR .EQ. 'B' ) THEN
                     CALL FERR ( INT2(319), &
     &                   'BATCH(arcset) Required baseline '// &
     &                   'weights  for session '//DBNAME_MES//' not found '// &
     &                   'in files '// &
     &                    WEIGHT_FILE(1)(1:I_LEN(WEIGHT_FILE(1)))//' '// &
     &                    WEIGHT_FILE(2)(1:I_LEN(WEIGHT_FILE(2)))//' '// &
     &                    WEIGHT_FILE(3)(1:I_LEN(WEIGHT_FILE(3)))//' '// &
     &                    WEIGHT_FILE(4)(1:I_LEN(WEIGHT_FILE(4)))//' ',  &
     &                    INT2(0), INT2(0) )
                ENDIF
               ELSE
!
! ------------- Weights requested, but not required
!
                IF ( G_WARNING ) THEN
                     WRITE  (  6, 110 ) DBNAME_MES, &
     &                                  WEIGHT_FILE(1:I_LEN(WEIGHT_FILE))
                     WRITE  ( 23, 110 ) DBNAME_MES, &
     &                                  WEIGHT_FILE(1:I_LEN(WEIGHT_FILE))
                ENDIF
             ENDIF ! LFLAG_FIND
!
! <<<<<<<< End of the old logic
!
         ELSE  ! the current logic
           IF ( WEIGHTS .EQ. 'R' ) THEN
                IUER = -1
             ELSE
                IUER = 0
           END IF
           CALL APPLY_WEIGHTS ( DBNAME, VER, NUMSTA, WEIGHT_TYPE_GEN, &
     &                          L_WEI, SUPNAM_WEI, SUPVER_WEI, BASELINE_WEI, &
     &                          ARR_WEI, IUER )
           IF ( IUER .NE. 0  .AND.( G_WARNING .OR.  WEIGHTS .EQ. 'R' ) ) THEN
                WRITE  (  6, 110 ) DBNAME_MES, &
     &                       WEIGHT_FILE(1)(1:I_LEN(WEIGHT_FILE(1)))//' '// &
     &                       WEIGHT_FILE(2)(1:I_LEN(WEIGHT_FILE(2)))//' '// &
     &                       WEIGHT_FILE(3)(1:I_LEN(WEIGHT_FILE(3)))//' '// &
     &                       WEIGHT_FILE(4)(1:I_LEN(WEIGHT_FILE(4))) 
                WRITE  ( 23, 110 ) DBNAME_MES, &
     &                       WEIGHT_FILE(1)(1:I_LEN(WEIGHT_FILE(1)))//' '// &
     &                       WEIGHT_FILE(2)(1:I_LEN(WEIGHT_FILE(2)))//' '// &
     &                       WEIGHT_FILE(3)(1:I_LEN(WEIGHT_FILE(3)))//' '// &
     &                       WEIGHT_FILE(4)(1:I_LEN(WEIGHT_FILE(4))) 
 110            FORMAT ( 'Warning: Arc or Site(s) for session ',A, &
     &                   ' are missing from weight files ',A/ &
     &                   'Possible reasons: a) there is no entry for ', &
     &                   'this Arc in the weight file; '/ &
     &                   'b) The number of stations in the session is ', &
     &                   'larger than the number of stations '/ &
     &                   'in the weights file')
                IF ( WEIGHTS .EQ. 'R' ) THEN
                     WRITE  (  6, '(A)' ) ' Nevertheless, continue '
                     WRITE  ( 23, '(A)' ) ' Nevertheless, continue '
                END IF
           END IF
           IF ( IUER .NE. 0  .AND.  WEIGHTS .EQ. 'R' ) THEN
                STOP 'BATCH(arcset) Abnormal termination'
           END IF
           END IF ! old_logic
         ELSE IF ( WEIGHTS .EQ. 'N' ) THEN
!
! -------- No reweighting. We put zero corrections to the weights
!
           CALL NOUT_R8 ( 4*INT4(MAX_ARC_BSL), CONSTANTS )
           CALL PUT_WEIGHTS ( CONSTANTS, 'A' )
      ENDIF
!
      CALL USE_GLBFIL ( 'ORC' )
!
! >>> Beginning the old logic
!
      IF ( OLD_LOGWEI ) THEN
           IF ( WEIGHT_TYPE_UR .EQ. 'S' ) THEN
                WEIGHTING_TYPE = 'ST'
             ELSE IF ( WEIGHT_TYPE_UR .EQ. 'A' ) THEN
                WEIGHTING_TYPE = 'DB'
             ELSE IF ( WEIGHT_TYPE_UR .EQ. 'B' ) THEN
               WEIGHTING_TYPE = 'BL'
           ENDIF
      ENDIF
!
! <<< End of the old logic
!
!
! --- Transorming weights flag to another notation
!
      IF ( WEIGHT_TYPE_GEN .EQ. 'A' ) WEIGHTING_TYPE = 'DB'
      IF ( WEIGHT_TYPE_GEN .EQ. 'B' ) WEIGHTING_TYPE = 'BL'
      IF ( WEIGHT_TYPE_GEN .EQ. 'S' ) WEIGHTING_TYPE = 'ST'
      ORIENT_OLD = ORIENT
      ORIENT = "Y"
!
! --- What kind of data ?
!
      LDTYPE=MODETYP  ! get local abvreviation of solution type
      IF ( LDTYPE(1:1) .EQ. ' ' ) LDTYPE=DTYPE ! Otherwise to get the global one
      IF ( LDTYPE(1:3) .EQ. 'GDR' ) THEN
           IDATYP = GRPRAT__DTP
         ELSE IF ( LDTYPE(1:3) .EQ. 'PDR' ) THEN
           IDATYP = PHSRAT__DTP
         ELSE IF ( LDTYPE(1:3) .EQ. 'GD ' ) THEN
           IDATYP = GRPONL__DTP
         ELSE IF ( LDTYPE(1:3) .EQ. 'PD ' ) THEN
           IDATYP = PHSONL__DTP
         ELSE
!
! -------- Search the token in the table of supported abbreviations. Shift
! -------- of the token in the table with respect to the first supported
! -------- abbreviation is just our solution type
!
           IDATYP = LTM_DIF ( 1, INT4(LAST__DTP-FIRST__DTP)+1, &
     &                        DATYP__ABR__ARR, LDTYPE ) + FIRST__DTP-1
           IF ( IDATYP .LT. FIRST__DTP ) THEN
                CALL FERR ( INT2(22010), &
     &              'BATCH(arcset): Unknown solution type  >'//LDTYPE// &
     &              '< for arc '//DBNAME, INT2(0), INT2(0) )
                STOP 'BATCH Abnormnal termination'
           END IF
      ENDIF
!
! --- Set suppression method
!
      SUPMET = SUPMET_BAT
      IF ( SUPMET == SUPMET__UND ) SUPMET = SUPMET_DEF ! ????
!
      IF ( SUPMET_ARC .NE. SUPMET__UND ) THEN
!
! -------- Overwrite suppression method, if specific suppression method
! -------- for this session was specified in the session options line
!
           SUPMET = SUPMET_ARC
      END IF
!
! --- Setting station deselection status flag
!
      CALL SET_STABIT ( INT2(2) )
!
      CALL SFLAGS ( STAFLG, FIXSTA_CHR, STADIU, VELFLG, SRCFLG, FIXSRC_CHR, &
     &     PROFLG, NUTFLG, UT1FLG, PRCFLG, RELFLG, &
     &     NRTARC, ROTFLG, ISTAD, KFAIL, ORIENT, ATMFLG, INTRVL, &
     &     CLKPOL_FLG, CLKPOL_DEG, CLKFLG, CKNTRVL, FCNPR, AXSFLG, &
     &     OFFLG, RATFLG, ACCEOP_FLG, IEOP_FLG, REOP_FLG, IEOPLL, FIXNAM, &
     &     BLCFLG, IOS_EST_BATCH, IOS_SIG_BATCH, NEXCBL, IBLNM, BASDF, EOPMID, &
     &     IONFLG, RESFILE, KMIN_SIG, GRADFLG, GRINTRVL, &
     &     KOUTNRM, KZERONRM, KSTACONST, KVELCONST, DO_CLK_REF, NUM_CLK_REF, &
     &     LIST_CLK_REF, EOP_SUPR, BASDEP_CLO, FL_NOMAP, DBNAME_MES, &
     &     EOP_EPOCH_MJD, EOP_EPOCH_SEC, EOP_BEFORE_SEC_TAI, EOP_AFTER_SEC_TAI  )
      CALL USE_GLBFIL   ( 'OWC' )
      CALL USE_GLBFIL_4 ( 'OWC' )
!
      EOPMID = EOPMID_SAVE 
      EOP_EPOCH_SEC = EOP_EPOCH_SEC_SAVE 
      EOP_EPOCH_MJD = EOP_EPOCH_MJD_SAVE 
!
      DO I=1,4
         FIXED_STA(I) = FIXNAM(I)
      ENDDO
      IF ( .NOT. NO_SUPERFILE ) THEN
!
! -------- Get julian date for the first and last observation of the session
!
           CALL OBSTM ( FIRST_JDATE, LAST_JDATE )
           CURJUL =   ( FIRST_JDATE + LAST_JDATE ) /2.D0
           DO I=1,NUMSTA
              VSITED(I) = 0.D0
              CALL HOL2CHAR ( ISITN(1,I), INT2(1), INT2(8), CISITN )
              CALL CHAR2HOL ( DBNAME, DBN, INT2(1), INT2(10) )
              DO J=1,NESM
                 IF ( ESMSITES(J) .EQ. CISITN  .AND. &
     &                CURJUL .GT. ESMDATES(J)        ) THEN
!
                      VSITED(I) = ESMDATES(J)
                 ENDIF
              ENDDO
           ENDDO
      ENDIF
!
! Set IBATCH bit array
!
!   Bit 1 = 1 then we are in Batch Mode
!   Bit 2 = 1 then minimum output
!   Bit 3 = 1 then print arc baseline info
!   Bit 4 = 1 then don't print terminal output
!   Bit 5 = 1 then don't print global parameters
!   Bit 6 = 1 then in a back solution
!   Bit 7 = 1 then print global baselines
!   Bit 8 = 1 then save this arc's commons in it's saved arcfile
!   Bit 9 = 1 then process covariances in this arc
!   Bit 10= 1 then globl output for ADJST AND BASFE, SET IN GLOBL
!   Bit 11= 1 then don't move OBSFIL
!   Bit 12= 1 then make resfil even though BATCH
!   Bit 13= 1 then print position error ellipses
!   Bit 14= 1 then this arc is fixed.
!
      IBATCH=0
      CALL SBIT ( IBATCH, INT2(1), INT2(1) )
      KBATCH = .TRUE.
      IF ( POSELL .EQ. 'Y' ) CALL SBIT ( IBATCH, INT2(13), INT2(1) )
      IF ( MINOUT .EQ. 'Y' ) CALL SBIT ( IBATCH, INT2(2), INT2(1) )
      IF ( BASOUT .EQ. 'Y'  .AND.  STAFLG .EQ. 'Y' ) THEN
           CALL LCSTA ( ILCSTA, NUMSTA, ISTAD, ISITN )
           IF ( ILCSTA .GT. 0  .OR. SOLTY2 .EQ. 'I' ) THEN
                CALL SBIT ( IBATCH, INT2(3), INT2(1) )
           END IF
           CALL GLSTA ( IGLSTA, I1, I2, NUMSTA, ISTAD, ISITN )
           IF ( IGLSTA.GT.0 ) CALL SBIT ( IBATCH, INT2(7), INT2(1) )
      ENDIF
      IF ( SCNOUT.EQ.'N' ) CALL SBIT ( IBATCH, INT2(4), INT2(1) )
      IF ( SOLTY2.EQ.'B' .AND. ARCNUM.NE.1 ) CALL SBIT ( IBATCH, INT2(5), &
     &     INT2(1) )
      IF ( SOLTY2.EQ.'B' ) CALL SBIT ( IBATCH, INT2(6), INT2(1) )
      IF ( USER_PROG .EQ. ' ' .OR. USER_PROG .EQ. 'NONE' ) THEN
           CALL SBIT ( IBATCH, INT2(11), INT2(1) )
      ENDIF
      IF ( WEIGHTS .EQ. 'A'  .OR.  WEIGHTS .EQ. 'M' ) THEN
           CALL SBIT ( IBATCH, INT2(11), INT2(0) )
      END IF
      CALL SBIT ( IBATCH, INT2(12), INT2(0) )
      IF ( ORIENT_OLD .EQ. 'N' ) CALL SBIT ( IBATCH, INT2(14), INT2(1) )
      IF ( FL_GVF ) THEN
!
! -------- No superfile in GVF mode!
!
           CALL SBIT ( PRE_IBATCH, INT2(11), INT2(0) ) 
      END IF
!
! --- We should write down parfil since its content may be changed for reading
! --- information about episodic site motion. SOCOM_EXT will rearead PARFIL
! --- from the disk
!
      CALL USE_PARFIL ( 'OWC' )
!
! --- Put SOCOM_PLUS in status "undefined" in order to force SOCOM_EXT
! --- to work
!
      SOCOM_PLUS_FIRST = SPL__UNDF
!
! --- Filling fields socom_plus and making test of eligibiulity of fast mode
!
      CALL SOCOM_EXT()
!
! --- Attempt to override FAST_MODE from environment variable and
! --- make test of eligibility FAST_MODE for this session
!
      IUER = -1
      CALL FAST_BYPASS ( IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4873, -2, 'ARCSET', 'Error in trying to bypass '// &
     &         'fast_mode' )
           CALL EXIT ( 1 )
           RETURN 
      END IF
!
! --- Last thing: call PARCN to update parameter counts and
!
      NUM_USER_PART = 0 ! At this point we do not know the number of user partl
      CALL PARCN()
      IF ( NPARAM .GT. NRMFL_PARMS ) THEN
           WRITE (  6, * ) 'ERROR:  BATCH(arcset) NPARAM = ',NPARAM, &
     &                    ' NRMFL_PARMS = ',NRMFL_PARMS
           WRITE (  6, * ) 'The total number of parameters, '// &
     &                     'global and local in '//DBNAME_MES//' exceeded '// &
     &                     'the limit'
           WRITE (  6, * ) 'Recomendation: run solve_reset and increase '// &
     &                     'the "maximum number of parameters"'
           WRITE ( 23, * ) 'ERROR:  BATCH(arcset) NPARAM = ',NPARAM, &
     &                    ' NRMFL_PARMS = ',NRMFL_PARMS
           WRITE ( 23, * ) 'The total number of parameters, '// &
     &                     'global and local in '//DBNAME_MES//' exceeded '// &
     &                     'the limit'
           WRITE ( 23, * ) 'Recomendation: run solve_reset and increase '// &
     &                     'the "maximum number of parameters"'
           CALL FERR ( INT2(8404), &
     &         'BATCH(arcset) -- total number of parameters '// &
     &         'exceeded the limit', INT2(0), INT2(0) )
           STOP 'BATCH abnormal termination'
      ENDIF
!
! --- Write down parfil which keeps the status of estimation configuration
!
      CALL USE_PARFIL ( 'OWC' )
!
! --- Set WVR masks
!
      CALL SET_MASK()
!
! --- Set calibrations, contributions, and mapping in NAMFIL
!
      CALL SETNAM ( FL_NOCAL, FL_NOCONT, FL_NOMAP, IONCTL, DBNAME_MES, &
     &              FL_GVF, GVH )
      CALL CLOSENAMFIL()
!
! --- Open and read the GLBFxx do mapping setup
!
      CALL CHAR2HOL     ( SOLTYP, ISOLTYP, INT2(1), INT2(1) )
      CALL DOMAPP       ( EOPMOD )
!
      IF ( ARCNUM .EQ. 1  .AND.  SOLTYP .EQ. 'F' ) THEN
           GLO_FJDOBS_MIN = 0.0D0
           GLO_FJDOBS_MAX = 0.0D0
      END IF
!
      CALL USE_COMMON   ( 'OWC' )
      STAREF = FIXSTA_CHR
      SRCREF = FIXSRC_CHR
      CALL USE_GLBFIL_4 ( 'OWC' )
      CALL USE_GLBFIL   ( 'ORC' )
!
      IF ( ARCNUM .EQ. 1 ) THEN
           ARCPE_WORKED = .FALSE.
           CRES_WORKED  = .FALSE.
!
           IF ( USER_PROG .NE. ' '  .AND.  USER_PROG .NE. 'NONE' ) THEN
                WRITE ( 23, '(A)' ) 'USER_PROG: '//USER_PROG
                IF ( USER_BUFF(1:1) .EQ. 'Y' ) THEN
                     WRITE ( 23, '(A)' ) 'USER_BUFF: '//USER_BUFF(2:)
                END IF
           END IF
           IF ( KUSER_PART ) THEN
                WRITE ( 23, '(A)' ) 'USER_PART_PROG: '//USER_PART_PROG
           END IF
      END IF
!
! --- NB: It is possible that the current arc is not the first, but ARCPE
! --- has not worked yet and has not created output CGM
!
      IF ( .NOT. ARCPE_WORKED .AND. IPASS.EQ.1 ) THEN
           INAMCG = CGMNMR
           IF ( CGMNMR(1:4) .EQ. 'NONE' .AND. TRIMLEN(CGMNMR) .EQ. 4 ) THEN
                INAMCG(1:4)='    '
           ENDIF
           IF ( CGMNMR(1:4) .EQ. 'SAVE' .AND. TRIMLEN(CGMNMR) .EQ. 4 ) THEN
                INAMCG(1:4)='    '
           ENDIF
         ELSE IF ( ( ARCPE_WORKED .AND. SOLTY2.EQ.'F' ) .OR. &
     &             ( ARCNUM.EQ.1  .AND. IPASS.EQ.2    )      ) THEN
!
! -------- Trick: we change the name of the input CGM on the fly in TRAIN mode.
! -------- The logic is different in NO TRAIN mode: input CGM matrix is read
! -------- before processing the first arc. so no specific action is done
! -------- in this case and we keep the nmame of the input CGM as a souvenir
!
           IF ( TRAIN ) THEN
                INAMCG = ONAMCG
           END IF
      ENDIF
!
! --- Output CGM
!
      IF ( SOLTY2.EQ.'B' .AND. ARCNUM.EQ.1 ) THEN
           IOCGM=0
           ISOLU=1
        ELSE IF ( SOLTY2.EQ.'F' .AND. KLAST ) THEN
           IOCGM=2
           ISOLU=0
        ELSE IF ( SOLTY2.EQ.'F' ) THEN
           IOCGM=1
           ISOLU=0
        ELSE IF ( SOLTY2.EQ.'I' ) THEN
           IOCGM=0
           ISOLU=0
      ENDIF
!
! --- Make residuals and forward output?
!
      IF ( SOLTY2.EQ.'F' .AND. FWDOUT.EQ.0 ) THEN
           ICONT=1
         ELSE IF ( SOLTY2.EQ.'F' .AND. MOD(ARCNUM,MAX(FWDOUT,1)) .NE.0 ) THEN
           ICONT=1
         ELSE IF ( SOLTY2.EQ.'G' .AND. MOD(ARCNUM,MAX(FWDOUT,1)) .NE.0 ) THEN
           ICONT=1
         ELSE IF ( SOLTYP.EQ.'C' .AND. ARCNUM .EQ. ITARCS ) THEN
           ICONT=1
         ELSE
           ICONT=0
      ENDIF
!
! --- set up correlation flags:
! --- I_ARC:  number of saved arcfile of i-arc
! --- Bit 8 of IBATCH = 1:  save this arc's commons
! --- Bit 9 of IBATCH = 1:  form correlation coefficients with
! ---                       this arc as j-arc
! --- CORLN:  execute the correlation program for this arc
! --- CORTP:  type of correlation
! --- PSPEC:  specific parameter to correlate, if any
!
      IF ( SOLTY2 .EQ. 'F' ) THEN
!
! -------- Forward pass
!
           CALL SBIT ( IBATCH, INT2(9), INT2(0) )
           CALL SBIT ( IBATCH, INT2(8), INT2(0) )
           IF ( ARCNUM .EQ. 1 ) I_ARC = 0 ! init in first arc of fwd dir.
           IF ( KCORL ) THEN
                ICOV(1:3) = QICOV(1:3)
                JCOV(1:3) = QICOV(1:3)
                IF ( IDBNAME(1:1) .NE. '$' ) THEN
                     I_ARCNAME(1:6) = IDBNAME(1:6)
              ENDIF
!
              IF ( I_ARCNAME(1:3) .NE. 'CGM' ) THEN
                     CALL SBIT ( IBATCH, INT2(8), INT2(1) )
                     IF ( IDBNAME(1:10) .EQ. DBNAME .AND. &
     &                    DECIMALTOINT( IDBNAME(11:12),ierr ) .EQ. VER ) THEN
                          I_ARC = ARCNUM ! leave arc number in glbcm
                     END IF
                ELSE
                         KCORL = .FALSE.
              ENDIF
              ELSE
           END IF
         ELSE IF ( SOLTY2 .EQ. 'B' ) THEN
!
! -------- Batch pass
!
           IF ( KCORL ) THEN
                ICOV(1:3) = QICOV(1:3)
                JCOV(1:3) = QICOV(1:3)
                IF ( IDBNAME(1:1) .NE. '$' ) THEN
                     I_ARCNAME(1:6) = IDBNAME(1:6)
              ENDIF
              IF ( IDBNAME(1:3) .EQ. 'CGM' ) THEN
                   KCORL = .FALSE.
                ELSE
                     CALL SBIT ( IBATCH, INT2(8), INT2(1) )
                     CALL SBIT ( IBATCH, INT2(9), INT2(1) )
                     IF ( IDBNAME(1:1) .NE. '$' ) THEN
                          I_ARCNAME(1:6) = IDBNAME(1:6)
                       ELSE IF ( IDBNAME(1:10) .EQ. DBNAME ) THEN
                          I_ARCNAME = DBNAME(1:10)//INTTODECIMAL(VER)
                       ELSE
                          CALL SBIT ( IBATCH, INT2(9), INT2(0) )
                     END IF
                ENDIF
             ELSE
                CALL SBIT ( IBATCH, INT2(8), INT2(0) )
                CALL SBIT ( IBATCH, INT2(9), INT2(0) )
           END IF
      END IF
!
! --- Solution ID
!
      IF ( ARCNUM .EQ. 1 ) THEN
           SOLUID_CHR = ID
      ENDIF
!
! --- Total number of arcs
!
      TARCS=ITARCS
!
! --- ARC_FILE save directories
!
      ARCDIR(1)  = B_ARCDIR(1)
      ARCDIR(2)  = B_ARCDIR(2)
      ARCDIR(3)  = B_ARCDIR(3)
      KPERMARC   = B_KPERMARC
      ORIENT_OLD =  "x"
      IF ( ORIENT_OLD .EQ. 'N' ) THEN
           ARCDIR(1) = ' '
           ARCDIR(2) = ' '
           ARCDIR(3) = ' '
      ENDIF
!
! --- Save the variables that define this arc
!
      IARCNM = ARCNUM
      IIPASS = IPASS
      ISLTY2 = SOLTY2
      SLAST  = KLAST
!
! --- Save overall solution type
!
      CALL CHAR2HOL ( SOLTYO, ISOLTYP, INT2(1), INT2(1) )
!
! --- Table output control
!
      CALL CHAR2HOL ( TBLOUT, ITBLOUT, INT2(1), INT2(1) )
!
! --- Remember the command file position, so it can be properly set for
! --- recover
!
      CALL CFPOS ( IARCRC )
!
! --- BUILT=.FALSE. means we just started this arc
!
      BUILT=.FALSE.
!
! --- set factor by which to multiply eop flyby covariance
! --- (instead of RSSing fixed value)
!
      EOP_FACTOR = EOPFACT
      MDORCTL    = MODCONTROL
      REQREF     = ( REFREQ .EQ. 'Y' )
      CALL USE_GLBFIL ( 'OWC' )
!
! --- Set PRE_IP(3) to IBATCH for passing to programs
!
      PRE_IP(3)  = IBATCH
      CALL CLOSENAMFIL() ! For the case
!
      RETURN
      END  SUBROUTINE  ARCSET  !#!#
