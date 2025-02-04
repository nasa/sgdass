      SUBROUTINE SECND ( ARR, B3DOBJ, CNSTROBJ, IRESTYP, JD_DUR_NOM, &
     &                   JD_DUR_ACT, LBUF_LEN, LBUF, IPTR, PAGEWID, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SECND PROGRAM SPECIFICATION
!
! 1.1 Among the main tasks of this routine are doing the flyby
!     mapping, applying calibrations, and calculating and writing
!     out delay residuals.
!
! 1.2 REFERENCES:
!
! 2.  SECND INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
      REAL*8 ARR(*)
      INTEGER*4   LBUF_LEN, IPTR, PAGEWID
      CHARACTER   LBUF(LBUF_LEN)*120
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'crecm.i'
      INCLUDE 'buff2.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'oborg.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'precm.i'
      INCLUDE 'resfl.i'
      INCLUDE 'fast.i'
      INCLUDE 'cals.i'
      INCLUDE 'cnstr.i'
      INCLUDE 'vtd.i'
      INCLUDE 'edc.i'
!
      REAL*8      DERIV(M_GPA,2)
      COMMON    / DERCM / DERIV
      TYPE ( CNSTR__STRU    ) :: CNSTROBJ
      TYPE ( VTD__OBS_TYPE  ) :: OBS_TYP
      TYPE ( VTD__TYPE      ), POINTER :: VTD_PTR
      TYPE ( EDC__TYPE      ) :: EDC
      TYPE ( TPD__TYPE      ) :: TPD
      TYPE ( TCN__TYPE      ) :: TCN(M__TCN)
      TYPE ( NERS__TYPE     ) :: NERS
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: CRES
!     CALLED SUBROUTINES:  STATS
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4   IXX, IYY, IUER
      INTEGER*2   NHUM(MAX_ARC_STA), NMET
      CHARACTER   JBUF*70
      REAL*8      JD_DUR_NOM, JD_DUR_ACT, TEMP, HUM(2048,MAX_ARC_STA)
      REAL*8      LAST_HUM_TIME(MAX_ARC_STA), AVG_HUMID(MAX_ARC_STA), &
     &            RMS_HUMID(MAX_ARC_STA), METSTAT(8), AVG_ATM(4,MAX_ARC_STA)
      CHARACTER   SITE_HUMID(MAX_ARC_STA)*8, MET_SITE*8, FCAL_NAMES(112)*8
      CHARACTER   BUF_AOC(MAX_OBS)*80, BUF_EDIT(MAX_OBS)*16, BUF_ADDW(MAX_OBS)*64, &
     &            BUF_DTEC(MAX_OBS)*128, BUF_EXT_ERR(MAX_OBS)*128
      INTEGER*2   IPELV(2), IPAZ(2)
      INTEGER*2   JCAVAL(MAX_ARC_STA), IDUMMY
      INTEGER*2   JCAFFL(7,MAX_ARC_STA), NFCAL, NAMSTA, IERR
      INTEGER*4   IOS, IER
      CHARACTER   FNAME*(NAME_SIZE), FNAMEA*(NAME_SIZE), FNAMEB*(NAME_SIZE)
      CHARACTER   VTD_CONF_USE*128
      INTEGER*4   FILDES, FILDES2
      INTEGER*2   NUMDB_I2, LDBNAM(5,15), IDBV(15), IDBE(15)
      INTEGER*4   IVER, OBCOUNTR, EVINT4, EVSTART4, RAN_SEED_I4
      CHARACTER   CDBNAM(15)*10, BUF*8 
      EQUIVALENCE (CDBNAM(1),LDBNAM(1,1))
      INTEGER*2   IRESTYP, MCAPL
      REAL*8      DOC, DOERR, RERR6, ROC, DOERR_RAW, TMIN, FJDOBS, LJDOBS
      REAL*8      DERR_RAW, RERR_RAW, DPHER_RAW
      REAL*8      FOBS, DD, DR, TIME, ADDW_FRQ, ADDW_SES_SCL
      REAL*8      LATS(MAX_ARC_STA), HEIGHTS(MAX_ARC_STA), AX_OFFS(MAX_ARC_STA)
      REAL*8      BARO_CALS(MAX_ARC_STA), BARO_HEIGHTS(MAX_ARC_STA)
      INTEGER*2   AX_TYPES(MAX_ARC_STA)
      INTEGER*4   IBLAS1, NBLAS, JB, JA, JS, IDBEND_SAVE(15)
      INTEGER*2   I, TRIMLEN
      INTEGER*2   ICUR_EO_PT,IEOP,IXY_U,IROT,IORD,IROTT,IL
      REAL*8      APP(2,2)
      INTEGER*2   IDB, II, ITEST, J, NOGOOD, IRUNW_SET, IDATYP_SAVE 
      INTEGER*4   IOBS, NOBS, KOBS, IDBGN, IDEND, JERR
      CHARACTER   IDLT*1, DLT_CHR*1, IPUNC*1, IUNC(14)*1
      CHARACTER   JD_TO_DATE*23
      INTEGER*4   MIND
      PARAMETER   ( MIND = 32 )
      INTEGER*4   LIND, IND(2,MIND)
      REAL*8      RRESARR(4), JD_NOM_FIRST, JD_NOM_LAST, SOLVE_SNR_MIN, &
     &            IONO_GPS, IONO_VLBI, ADDW_EXT(MAX_OBS), &
     &            DTEC_GPS_EXT(MAX_OBS), EXT_ERR(MAX_OBS), &
     &            DTEC_ADJ_EXT(MAX_OBS), DTEC_ERR_EXT(MAX_OBS), &
     &            DEL_BIAS_UL_EXT(MAX_OBS), TAU_O, TAU_IGX, &
     &            TAU_IGS, DEL_DAT, DEL_MOD, FREQ_K, VTD_IONO_SCALE, &
     &            ELEV_DWNT, DWNT_FACTOR, EPS_SEC
      PARAMETER  ( DWNT_FACTOR = 1000.0D0 ) ! How the formal uncertainty should be scaled for downweighting
      PARAMETER  ( EPS_SEC = 0.1 ) ! max acceptable diff. in order of observ.
      INTEGER*2   IRESARR(26)
      EQUIVALENCE (RRESARR(1), IRESARR(11))
      COMMON  / PARTFILE /  FNAME, FILDES
      CHARACTER FNAMED*(NAME_SIZE), EDBSL*17, COMPBSL*17, EDSRC*8, COMPSRC*8, &
     &          FATFIL*128, FINAM_DEBUG*128, SOLVE_DIR_DEBUG*128
      INTEGER*8  LEN_WEI_CNS
      INTEGER*4 ITMP1, ITMP2, ITMP3, ITMP4, IBAS4, IS, &
     &          L_TCN, LUN_DEBUG, L_SCA, K_SCA(MAX_SCA), &
     &          ITIM_UPWEI_BEG, ITIM_UPWEI_END, MJD_1ST 
      INTEGER*2 EDYR, EDMN, EDDAY, EDHR, EDMIN, IY, IM, ID, IHR, IMIN, ITIME, &
     &          ISTA
      REAL*8    PHI_GCN, PHI_GDT, LAMBDA, H_ELL, RD, G_ACC, UTC_1ST 
      REAL*8    TIM_UPWEI_BEG, TIM_UPWEI_END, &
     &          SCA_TIM(MAX_ARC_SRC), SCA_DUR(MAX_ARC_SRC), &
     &          SCA_TIM_USED(MAX_ARC_SRC), SCA_DUR_USED(MAX_ARC_SRC), ERR_FLOOR
      CHARACTER SOLVE_DEBUG*128, STS_STR*2, SOLVE_SNR_MIN_STR*128
      CHARACTER CBUF*80, IORE*3, IORECUR*3, STA_NAM(2)*8
      REAL*8     DEL_GPS_IONO(2), IONO_ZEN_AVR(2), IONO_ZEN_COV(3), AOC_DEL(MAX_OBS), &
     &           TAU_E_MAX, WEI
      TYPE ( CALS_STRU ) ::  CALS
      LOGICAL*2 KBIT, POLYONLY
      LOGICAL*2 CHECK_EDIT, EDIT_OPENED, FL_SOU_DESL, FL_BAS_DESL, FL_USED_AS_XS, FL_USED_AS_X, &
     &          FL_USED_GXS, FL_USED_GS, FL_USED_GX
      LOGICAL*1 FL_VTD_IONO, FL_SUBST, FL_NOMECHI, FL_ADDW_IONO, FL_ADDW_BW, FL_EXT_ERR
      LOGICAL*4 NTD_FL(MAX_ARC_STA),  NTW_FL(MAX_ARC_STA)
      LOGICAL*4 FL_TPD_DEBUG, FL_TPD_READ, FL_DEBUG_SBA
      INTEGER*4 TPD_USE_FLAG
      INTEGER*4 NTD_IND(MAX_ARC_STA), NTW_IND(MAX_ARC_STA)
      INTEGER*2 INT2_ARG
      INTEGER*4 INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      CHARACTER, EXTERNAL :: GET_CDATE*19, MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: GET_UNIT
      LOGICAL*4, EXTERNAL :: BAD_OBS
      REAL*8,    EXTERNAL :: DDOT, RGAUSS
!
! --- added JMGipson 970227  \\
!
      INTEGER*4 MAXC
!
! --- Number of non-zero elements in constraint matrix
!
      PARAMETER ( MAXC=10*M_GPA )
      REAL*8      CHI_REDUCE
!
! --- Post_fit correction factor function
!
      REAL*8     DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), DELAY_THR, &
     &           RATE_THR, PRES_VAL, TEMP_VAL, UTC_OBS, UTC_MINUS_TAI, &
     &           TAI_OBS, TAI_OBS_BEG, UTC_M_TAI_SAVE
      REAL*8     POST_FIT_COR,  POST_FIT_DEL_COR, POST_FIT_RAT_COR, COEF_IONO
!
! end JMGipson 970227  //
!
      DATA IUNC / ' ', 'H', 'E', 'F', 'I', 'T', 'S', &
     &            'R','A', 'C', 'P', 'Q', '?', 'V' /
      REAL*8     RES__MAX
      PARAMETER  ( RES__MAX = 1.0D0 ) ! 1 second
      INTEGER*4  FC_LSEEK, FC_WRITE
      ADDRESS__TYPE :: PTR_NC
      REAL*8,    EXTERNAL :: ZENDEL_SAA 
!CCCCC
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  860924  Eliminated some code which was duplicated for first and
!                second observation of a differenced pair.  Use GOTO's and
!                a flag (DIFFLG) to send CRES to the same piece of code
!                for each observation in the pair
!   JRR  890113  Added menu options (special features) and code to
!                implement selected operations
!   CEK  890206  Add in elevation cutoff options by site, iunw=3.
!                Add in WVR masking by site, iunw=12.  Note that all sites
!                are checked against elevation cutoff elvcut array in
!                SOCOM, and all are also checked against WVR mask.  The
!                default values for both elevation and WVR are 0.  A
!                WVR mask of zero "passes" all data whether WVR data
!                exists or not.
!   CEK  890207  Site dependent elevation cutoff logic
!   MWH  900416  Modify to accomodate Lanyi model
!   MWH  910524  Add flag to incicate only polynomial clock parameters
!                    (i.e. no stochastics)
!   AEE  911116  Moved call to SOCAL before the call to PARTL.
!   AEE  920628  added call to ATMPART.
!   jwr  931214  Source pointer arrays intialized so that the list
!                of source observation fits come out in ra order.
!   mwh  940512  Alphabetize stations within baseline names
!   jwr  950801  Error in setting the iunwp flag to 8 fixed. Changed
!                the units on the delay and rate residuals to ps and fs/s
!   kdb  950831  Add atmospheric turbulence rate constant and mapping function
!                parameter error delay and rate constants.
!   kdb  951207  Integer*4 number of observations.
!   pet  970130  Added in listing information about fast_mode. Fixed bug in
!                screen mode printout
!   pet  970207  Bug fixed: Some fields of B3DOBJ had not been initializing
!   jmg  970227  Modified to calculate post-fit sigmas.
!   pet  970228  Time profiling feature added
!   jmg  970310  Modified to avoid rounding problem which resulted in NaN
!                when doing post-fit sigmas.
!   pet  970318  Updated comments
!   pet  970319  Made calculation correction of postfit residuals only in
!                case when residual file is opened.
!   jwr  970320  All code relative to the file of high frequency eop
!                information deleted.  This logic has been moved to
!                a2jst_neweop.f in ADJST.
!   pet  970604  Fixed bug for the case when several databases analyzed.
!   pet  970606  Changed logic for restoration calibrations saved by PROC
!   pet  970712  Supress printout when it called from REWAY
!   pet  971029  Added setting flag in the bit field IDBEST to signal that
!                IDB-th database in the database-list has been processed and
!                prodiced residuals
!   pet  971124  Accomodated changes in the set of formal parameters for NCORT
!   pet  971202  Added logic for bypassing deselected station
!   pet  971204  Added logic for splitting baseline deselection on phase delay
!                and group delay solution type
!   pet  980203  Substituted hard-coded test of solution type by DATYP_INQ
!   pet  980219  Removed call of socal and put instead of it calls get_calib and
!                make_oc -- they support extended list of solution type.
!   pet  980227  Made some code modification to allow to use new phase delay
!                o-c type.
!   pet  980330  Added calculation of extended statistics about number of used
!                recoverable, unrecoverable, suppressed observations.
!   pet  980429  Changed logic for gathering statistics:
!                1) Made calls of SUPR_INQ instead of analasyng IUNW codes
!                   in order to determine status of the onbservation;
!                2) Put in baseline and source statistics only observations
!                   which actually participated in solutiuon;
!                3) Changed scheme for counting participants for extended
!                   statisitcs
!   pet  980920  Added calculation actual and nominal duration of the session
!   pet  980921  Corrected S-band related bug. Added a special treatment of
!                the formal error os ionosphere free linear combination for
!                the case when effective frequency corresponds to S-band
!   pet  990108  Added a call of FLYBY_MAP_INIT
!   pet  990129  Added a check whether effective frequency is Not-A-Number
!   pet  990312  Got rid from unused variables
!   pet  990405  Added a call of GET_TIM
!   pet  990420  Prohibited to spoil statistics for extremly bad observations.
!                If postifit residual exceeds RES__MAX second in modulo it is
!                set to RES__MAX seconds. It is intended to bar appearance of
!                +INF in statistics.
!   pet  990421  Forced loop to bypass calibration and computation of
!                atmosphere partials if there were no fringes at least at
!                one bad (fuality codes are 0 or letter) to prevent error
!                messages in attempt to handle observations with negative
!                elevation angles
!   pet  1999.05.28 Made LBUF_LEN, LBUF, IPTR, PAGEWID formal arguments,
!                   eliminated common block cres_buf
!   pet  1999.06.08   Changed reaction on NaN for frequencies. Changed order of
!                     operations near call make_oc.
!   pet  1999.11.17   Updated the list of actual parameters for NCORT and
!                     GET_CALIB
!   pet  2000.01.25   Added support of NORATE_FLAG. Rate statistic is bypassed
!                     and not computed when NORATE_FLAG is .TRUE. and the
!                     program runs faster.
!   pet  2000.05.02   Added a new feature: the subroutine writes a priori
!                     zenith path delay in the file
!                     $WORK_DIR/ATZE{solve_initials} if the variable
!                     APRIORI_ZENDEL is .TRUE.
!   pet  2001.01.11   Eliminated traces of the code for support of differenced
!                     observations. Added computation of ME_CHI -- mathematical
!                     expectation of the sum of squares weighted residuals
!   pet  2001.05.17   Fixed the bug: the previous version was trying to compute
!                     ME_CHI in global mode.
!   pet  2001.06.19   Slightly changed format of residuals file: abolished
!                     shortening station names to two characters and extended
!                     date format to a tenth of a second. Changed logic of
!                     showing residuals in interactive Solve. Now residuals
!                     are printed on screen if "spool current is off".
!                     Alternatively, residuals are written in spool file,
!                     but not on screen, if "spool current is on".
!   pet  2001.07.10   Added logic for checking whether calibrations are
!                     not-a-number.
!   pet  2001.08.01   Restored printing the counter of computed residuals
!                     which was accidentally lost in previous updates.
!   pet  2001.12.19   Added new field in residual file: COEF_IONO. COEF_IONO is
!                     f_gx**2/(f_gx**2 -f_gs**2) and it is the factor of
!                     groupd delay at X-band in an ionosphere free linear
!                     combination of group delay observables
!   pet  2002.02.13   Added a trap of internal control for the case when DERR
!                     is -INF or +INF
!   pet  2002.03.05   Changed the logic for computing apriori zenith path delay.
!                     The previous version call subroutine SASTD. However,
!                     this way of computation ignores that facts that
!                     1) Solve can "fix" bogus meterological values used
!                        to show that no meteorological information is
!                        available for a specific station;
!                     2) Solve can apply both wet and dry apriori zenith path
!                        delay.
!                     The new version keeps the value of apriroi zenith path
!                     delay used by Solve and puts into the ATZExx file
!   pet  2002.03.06   Added tracing the situation when Calc-supplied troposphere
!                     contribution was supplied and user requested printout of
!                     apriori zenith path delay. the new version prints warning
!                     when the option "WARNING ON" is set. Currently, Solve is
!                     not in a position to learn what was the apriori zenith
!                     path delay when non-flyby zenith path delay was used.
!   pet  2002.06.05   Added code which computes the weighted average dates
!                     of observations for any specific station and for all
!                     stations togeather.
!   pet  2002.06.11   Fixed the bug in the code above
!   pet  2003.08.19   Fixed a bug: the privious version tried to work with
!                     unitialized variables in the case if the first observation
!                     of the sessions is non-detection. In rary cases it
!                     may result to float point execption and abnormal
!                     termination
!   pet  22-AUG-2003  Fixed a long lived bug: variable SKIP_COUNT should be &
!                     initialized since it is used by PARTL
!
!   pet  2007.10.26   Added support of external decimation files
!
!   pet  2007.11.08   Added support of external theoerical path delay files
!   pet  2009.07.28   Added support of printing output apriori zenith path &
!                     delay for VTD mode
!   pet  2010.02.28   Added support of the kludge variable SOLVE_SNR_MIN
!   pet  2010.10.22   Added support of TCN -- Tec Noise File -- additional &
!                     noise added in quadrature to reciprocal weights in order
!                     to account for errors in a GPS TEC model
!   pet  2010.11.21   Corrected buf related to computation ot ATM_ZENDEL 
!   pet  2017.11.11   Corrected computation of rms of atmospheric parameters
!                     in order to avoid sqrt of a negatice number due to rounding
!   pet  2018.09.08   Added support of IONO_ERR_FCT variable for inflating group delay
!                     uncertainty in order to accommodate ionosphere path delay errors
!   pet  2018.09.08   Fixed a crash in the listing printout mode when the residuals
!                     are very large numbers, like -1.D19
!   pet  2018.12.23   Added support of ERR_FUDGE_FACTOR defined as an ARC option
!   pet  2019.10.18   Made changes in order to support new interface to GET_UTC_M_TAI
!   pet  2020.04.27   Implemented support of an edit file with external suppression flags
!   pet  2020.07.15   Added support of the external additive weight correction passed via
!                     file defined as session-specific option.
!   pet  2021.03.17   Added support of external apriori observation file
!   pet  2021.03.18   Fixed a bug: check for BAD_OBS was missing
!   pet  2021.12.30   Disabled support of ERR_FUDGE_FACTOR and replaced it with
!                     support of SESS_REWEI_SCALE and SESS_REWEI_QUADR parameters
!                     defined an ARC option
!   pet  2021.12.31   Added support of external dTEC files that define dTEC, dTEC error and
!                     ionosphere-free bias of the group delay at the upper band with
!                     respect to the lower band
!   pet  2022.02.14  Added support of fused data type
!   pet  2022.06.08  Added support of adjustement to ionospheric path delay 
!   pet  2022.06.26  Added support of TEC_SCAL and TEC_BIAS
!   pet  2023.01.06  Added support of envirnoment variable that sets the use of &
!                    single band dTEC adgjusments and scales its errors
!   pet  2023.01.06  Added support of envirnoment variable STR_DTEC_SBA_ERR_SCL 
!                    that sets the use of single band dTEC adgjusments and scales 
!                    its errors
!   pet  2023.02.10  Added support of external error file
!   pet  2023.07.24  Added more digits to residuals in the spool file
!   pet  2023.07.31  Fixed a 23-year old bug: a hard-coded LUN was used when
!                    APRIORI_ZENDEL was set
!   pet  2023.08.01  Fixed '******' in MDAT error message
!   pet  2023.08.01  Fixed '******' in MDAT error message
!   pet  2023.08.23  Fixed '******' in MDAT error message in another place
!   pet  2024.07.09  Added support for computation of scan statistics and weighted epoch
!                    for a given source
!
!CCCCC
!
! 5.  SECND PROGRAM STRUCTURE
!
!     If menu option was selected in OPTIN (IMENU=1) then do it here.
!
!CCCCC
      LOGICAL*2  K_MN
      LOGICAL*1  FL_ADDW, FL_AOC, FL_EDIT, FL_DTEC, FL_SUP(MAX_OBS)
      INTEGER*4  MJD_UTC_OBS, MJD_TAI_OBS, MJD_TAI_OBS_BEG 
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           I61, NE, NA, N_WEI, N_DTEC, N_EXT_ERR, LUN_AZ, IND_OBS
      INTEGER*8  SIZEOF_VTD, MEM_LEN
      ADDRESS__TYPE :: ADR_WEI_CNS, MEM_LEN_VTD, MEM_ADR_VTD, MEM_ADR
      CHARACTER  STR*32, STR_DATE*21, FINAM_FAST*80, &
     &           C_STA(MAX_ARC_STA)*8, STR_DTEC_SBA_ERR_SCL*32
      REAL*8     TAU_CALC,      RATE_CALC,     COR_TAU,       COR_RATE, &
     &           ADDERR_GR_TAU, ADDERR_PH_TAU, ADDERR_RATE, &
     &           TAUGR_OBS_X,   TAUGR_OBS_S,   TAUPH_OBS_X,   TAUPH_OBS_S, &
     &           TAUSB_OBS_X,   TAUSB_OBS_S,   TAUGR_ERR_X,   TAUGR_ERR_S, &
     &           TAUPH_ERR_X,   TAUPH_ERR_S,   TAUSB_ERR_X,   TAUSB_ERR_S, &
     &           RATE_OBS_X,    RATE_OBS_S,    RATE_ERR_X,    RATE_ERR_S, &
     &           FREQ_GR_X,     FREQ_GR_S,     FREQ_PH_X,     FREQ_PH_S, &
     &           FREQ_RATE_X,   FREQ_RATE_S,   TAU_OC,        RATE_OC, &
     &           TAU_E, RATE_E
      REAL*8     DT_SAVE, RT_SAVE, DERR_SAVE, RERR_SAVE, DPHER_SAVE, &
     &           WW_STA(MAX_ARC_STA), WW_ALL, BIG_VALUE
      PARAMETER  ( BIG_VALUE = 1.D10 )
      REAL*8     VC1(M_GPA), VC2(M_GPA), SPARSE_DOT
      REAL*8     COMPUTE_MECHI
      PARAMETER  ( ERR_FLOOR = 1.D-12 ) ! Error floor
      TYPE ( PLACE__STRU )  ::  PLACE
      TYPE ( B3D__STRU   )  ::  B3DOBJ
      TYPE ( B1B3D__STRU )  ::  B1B3DOBJ
      LOGICAL*4  FL_USED, FL_RECO, FL_GOOD, FL_SUPR, FL_INIT, FL_SNR
      LOGICAL*4, EXTERNAL :: CHECK_STABIT, DATYP_INQ, SUPR_INQ, META_SUPR_INQ, &
     &                       IS_R8_NAN
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!CCCCC
!
! --- Setting flag of printing the ouput intformation at the screen usiing
! --- "_MN" interface (curses)
!
      K_MN = KSCREEN                     .AND.KBIT ( PRE_IP ( 2 ), INT2(6))           ! Interactide mode
      IF ( KBIT ( PRE_IP ( 3 ), INT2(12)) .AND.REWAY_VERBOSE )  &
     &     K_MN = .FALSE.  ! But suppress printout
!                                               !  in silent REWAY mode
      CALL GETENVAR ( 'DEBUG_SBA', STR )
      IF  ( STR == 'CRES' .OR. STR == 'cres' ) THEN
            FL_DEBUG_SBA = .TRUE.
         ELSE
            FL_DEBUG_SBA = .FALSE.
      END IF 
!
      TAU_E_MAX = 1.0D0
      CALL GETENVAR ( 'SOLVE_TAU_E_MAX', STR ) 
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F10.5)' ) TAU_E_MAX
      END IF
!
      CALL GETENVAR ( 'SOLVE_ELEV_DOWNEIGHT', STR )
      IF ( ILEN(STR) > 0 ) THEN
           IF ( INDEX ( STR, '.' ) < 1 ) THEN
                STR = TRIM(STR)//'.0'
           END IF
           READ ( UNIT=STR, FMT='(F10.3)' ) ELEV_DWNT
           ELEV_DWNT = ELEV_DWNT*DEG__TO__RAD
         ELSE 
           ELEV_DWNT = -1.0D0
      END IF
!
      CALL CLRCH    (                     STR_DTEC_SBA_ERR_SCL )
      CALL GETENVAR ( 'DTEC_SBA_ERR_SCL', STR_DTEC_SBA_ERR_SCL )
      IF ( ILEN(STR_DTEC_SBA_ERR_SCL) .NE. 0 ) THEN
           IF ( STR_DTEC_SBA_ERR_SCL(1:1) == 'N' ) THEN
                DTEC_SBA_USE = .FALSE.
              ELSE 
                DTEC_SBA_USE = .TRUE.
                READ ( UNIT=STR_DTEC_SBA_ERR_SCL, FMT=* ) DTEC_ERR_SCL
           END IF
      END IF
!
      IS = 294715
!
      JA = 1 + 3*M_GPA
      JB = 1 + 2*M_GPA
      JS = 1 + 1*M_GPA
      POLYONLY = .TRUE.
      DO I=1,NUMSTA
!
! ------ Check STABIT_P or STABIT_G bit fields to bypass deselected station
!
         IF ( .NOT. CHECK_STABIT ( I ) ) GOTO 810
         DO J=1,NUMCLK(I)
            IF ( KBIT( LCLK(J), INT2(13)) .AND. KBIT(ICLSTA(1,J),I) ) THEN
                 POLYONLY = .FALSE.
            ENDIF
         ENDDO
 810     CONTINUE
      ENDDO
!
      FREQ_K = 23.5D9
      FL_SUBST = .FALSE. 
      CALL GETENVAR ( 'SOLVE_SUBST', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) FL_SUBST = .TRUE.
!
      CALL GETENVAR ( 'SOLVE_DEBUG', SOLVE_DEBUG )
!
      CALL CLRCH    ( SOLVE_SNR_MIN_STR )
      CALL GETENVAR ( 'SOLVE_SNR_MIN', SOLVE_SNR_MIN_STR )
      IF ( ILEN(SOLVE_SNR_MIN_STR) > 0 ) THEN
           READ ( UNIT=SOLVE_SNR_MIN_STR, FMT='(F20.0)', IOSTAT=IER ) SOLVE_SNR_MIN
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8910, IUER, 'SECND', 'Wrong format of '// &
     &              'environment variable SOLVE_SNR_MIN: '//SOLVE_SNR_MIN_STR )
                RETURN 
           END IF
        ELSE 
           SOLVE_SNR_MIN = 0.0D0
      END IF
!
      CALL GETENVAR ( 'SOLVE_USED_AS_XS', STR )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_USED_AS_XS = .TRUE.
         ELSE 
           FL_USED_AS_XS = .FALSE.
      END IF
      CALL GETENVAR ( 'SOLVE_USED_AS_X', STR )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_USED_AS_X = .TRUE.
         ELSE 
           FL_USED_AS_X = .FALSE.
      END IF
!
      CALL GETENVAR ( 'SOLVE_TIME_UPWEIGHT', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL EXWORD ( STR, MIND, LIND, IND, '_', IER )
           IF ( LIND .LT. 5 ) THEN
                CALL ERR_LOG ( 8911, IUER, 'SECND', 'Malformed variable '// &
     &              'SOLVE_TIME_UPWEIGHT: it should have 5 words separated '// &
     &              'by _' )
                RETURN 
           END IF
           CALL CHIN ( STR(IND(1,2):IND(2,2)), ITIM_UPWEI_BEG )
           CALL CHIN ( STR(IND(1,4):IND(2,4)), ITIM_UPWEI_END )
           IF ( STR(IND(1,5):IND(2,5)) == 'SEC' ) THEN
                TIM_UPWEI_BEG = ITIM_UPWEI_BEG 
                TIM_UPWEI_END = ITIM_UPWEI_END
             ELSE IF ( STR(IND(1,5):IND(2,5)) == 'MIN' ) THEN
                TIM_UPWEI_BEG = 60.0D0*ITIM_UPWEI_BEG 
                TIM_UPWEI_END = 60.0D0*ITIM_UPWEI_END
             ELSE IF ( STR(IND(1,5):IND(2,5)) == 'HR' ) THEN
                TIM_UPWEI_BEG = 3600.0D0*ITIM_UPWEI_BEG 
                TIM_UPWEI_END = 3600.0D0*ITIM_UPWEI_END
           END IF
!
           WRITE (  6, * ) 'SECND SOLVE_TIME_UPWEIGHT is set up: TIM_UPWEI_BEG/TIM_UPWEI_END= ', SNGL(TIM_UPWEI_BEG), SNGL(TIM_UPWEI_END)
           WRITE ( 23, * ) 'SECND SOLVE_TIME_UPWEIGHT is set up: TIM_UPWEI_BEG/TIM_UPWEI_END= ', SNGL(TIM_UPWEI_BEG), SNGL(TIM_UPWEI_END)
         ELSE
           TIM_UPWEI_BEG = -1.D8
           TIM_UPWEI_END =  1.D8
      END IF
!
! --- Set the delay reweight floor to 15 ps.
!
      REWEIGHT_FLOOR = 15.E-12
!
      NBLAS=NPARAM
      IBLAS1 = 1
!
      IBCNT  = 0
      IDUMMY = 0
      ISCNT  = NUMSTR
!
! --- Initialize isrc array so source list comes out in ra order.
!
      DO I=1,NUMSTR
         ISRC(1,I) = I
         DO J=2,4
            ISRC(J,I) = 0
         ENDDO
         NSCA_TOT(I)  = 0
         NSCA_USED(I) = 0
      ENDDO
!
! --- Zero out statistical variables
!
      WRMSI(1) = 0.D0
      WRMSI(2) = 0.D0
      FACT(1)  = 0.0D0
      FACT(2)  = 0.0D0
      SCA_TIM  = -1.D20
      SCA_DUR  = 0.0
      SCA_TIM_USED   = -1.D20
      SCA_DUR_USED   = 0.0
      WEIGHTED_EPOCH = 0.0
      WEIGHT_SUM     = 0.0
      NC=0
      KC=0
      CALL NOUT_I4 ( 3*INT4(MAX_ARC_SRC), ISTAT_SRC )
!      IF ( ISLTY2 .EQ. 'I' ) THEN  ! only in INDEPENDENT solution type
!
! --------- Read in constraint matrix. This is generated by proc
!
!           CALL READ_SPARSE   ( CVAL, IXIND, IYIND, MAXC, NUMC )
!      END IF
!
! --- Initialization for code which writes plot file of earth orientation
! --- mdlpl-style adjustments
!
! --- First, miscellaneous initialization
!
      IF ( IEOPL .NE. 0 ) THEN
           EOPLOT_CT = 0
           EOPLOT_CNVRT(1) =(180.0D0/PI__NUM) *3600.0D0 *1000.D0 !rad-> mas for wobble
           EOPLOT_CNVRT(2) = EOPLOT_CNVRT(1)
           EOPLOT_CNVRT(3) = 1.0D3 ! sec to ms for UT1
!
! -------- Then find locations of earth orientation parameters in DERIV array,
! -------- etc.
!
           ICUR_EO_PT = NPARAM - IPSTP + 1 !This is where the eo parameters
           DO IEOP = 1,3
              IF ( IEOP .LE. 2 ) THEN
                   IXY_U = 1
                ELSE
                   IXY_U = 2
              END IF
              EOPLOT_STARTS(IEOP) = ICUR_EO_PT
              IF ( EOP_STYLE(IXY_U) .EQ. 1 ) THEN
                   EOPLOT_NUMS(IEOP) = 2 !offset and global rate
                   IF ( KBIT ( EOPA1_CHOICE(IXY_U), INT2(1) ) ) THEN !
!
! -------------------- one rate break per epoch
!
                       EOPLOT_NUMS(IEOP) = EOPLOT_NUMS(IEOP) + &
     &                                     NROT_A1(IXY_U) - 1
                   END IF
                   DO J = 1,2 !diurnal, then semi-diurnal
                      IF ( KBIT ( EOPA1_CHOICE(IXY_U), INT2(J+1)) ) THEN
!
! ------------------------ sine,cos amps for this type of sine
!
                           EOPLOT_NUMS(IEOP) = EOPLOT_NUMS(IEOP) + 2
                      END IF
                  END DO
               ELSE
                  EOPLOT_NUMS(IEOP) = 0
                  DO IROT = 1,NROT
                     DO IORD = 1,4
                        IF ( IROTT( IROT, IEOP, IORD, LROT ) .EQ. 1) &
     &                       EOPLOT_NUMS(IEOP) = EOPLOT_NUMS(IEOP) + 1
                     END DO
                  END DO
              END IF
              ICUR_EO_PT = EOPLOT_STARTS(IEOP) + EOPLOT_NUMS(IEOP)
           END DO
      END IF
!
      EVINT4 = EVINT
      EVSTART4 = EVSTART
!
! --- This trick is done in order to overcome a bug in HP Fortran90 2.5.1
! --- whcih erorneosuly puts MOD(I,0) out of cycles and dies due to
! --- IEEE dividing by zero
!
      IF ( EVINT4 .LE. 0 ) EVINT4 = MAX_OBS + 1
!
      CALL DBPOX ( NUMDB_I2, LDBNAM, IDBV, IDBE )
      IDBEND_SAVE = IDBE
      CALL GET_AVG_ATM ( CDBNAM(1), AVG_ATM )
!
! --- Initialize some statistics counters
!
      ITOTRC_OBS = 0  ! Number of potentially recoverable observations
      ITOTCG_OBS = 0  ! Number of of conditionally good observations
      ITOTUS_OBS = 0  ! Number of used observations
      ITOTSU_OBS = 0  ! Number of suppressed observations
!
      IDBGN = 1
      NDIFF = 0
      NUMBER = 0
!
      DO 1010 IDB = 1, NDB
         DO I=1,MAX_ARC_STA
            AVG_HUMID(I) = 0.d0
            RMS_HUMID(I) = 0.d0
            NHUM(I) = 0
         ENDDO
         DO I=1,NUMSTA
            CALL HOL2CHAR ( ISITN(1,I), INT2(1), INT2(8), SITE_HUMID(I) )
         ENDDO
         IDEND = IDBEND(IDB)
         IF ( .NOT. KBIT(IDCSEL,IDB) ) GOTO 1009
!
! ------ Setting IDB-th bit "the database has been estimated and produced
! ------ residuals"
!
         IF ( KBIT(IDBSEL,IDB) ) CALL SBIT ( IDBEST, IDB, INT2(1) )
!
! ------ Read station names and status array
! ------ and set up a correspondence table between the stations
! ------ listed in NAMFIL (JSIT's) and those listed in PARFIL (ISIT's)
!
          CALL NCORT ( JSITN, JSITI, JCAPPL, NUMSTA, ITT, IDB, &
     &                 IDATYP, ITTB, ET, SE, SS, OBCAPL, MCAPL, JCAVAL, &
     &                 LATS, HEIGHTS, AX_TYPES, AX_OFFS, BARO_CALS, &
     &                 BARO_HEIGHTS, JCAFFL, FCAL_NAMES, NFCAL, NAMSTA, CALCV )
!
          IF ( K_MN .AND. IAND(IPRES,INT2(1)) .EQ.0 ) THEN
               IPTR=IPTR+1
               CALL ADDSTR_F ( "  Residuals from data base "//CDBNAM(IDB) )
          ENDIF
!
! ------- Open user_partial scratch file, if there is one
!
          IF ( KUSER_PART .AND. ( ARC_USER_PART > 0 .OR.  NUM_USER_PART > 0 ) ) THEN
               FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'PART'//PRE_LETRS
               CALL BIN_OPEN ( FNAME, FILDES, 'O' )
          ENDIF
          IF ( RESOUTFILE(1:4) .NE. 'NONE'  .AND.  KBATCH ) THEN
               IL=TRIMLEN(RESOUTFILE)
               IF ( IL .LE. 0 ) IL = 1
               FNAMEA = RESOUTFILE(:il)//'.ndx'
               CALL USE_RESNDX ( 'O', FNAMEA )
               WRITE ( 37, '(3I6)', IOSTAT=IOS ) IARCNM, NUMSTA, NUMSTR
               CALL FERR ( INT2(IOS), "Writing residual file", INT2(0), &
     &              INT2(0) )
               DO I=1,NUMSTA
                  WRITE ( 37, '(A8)', IOSTAT=IOS ) ISITN_CHR(I)
                  CALL FERR ( INT2(IOS), "Writing residual file", INT2(0), &
     &                 INT2(0) )
               ENDDO
!
               DO I=1,NUMSTR
                  WRITE ( 37, '(A8)',IOSTAT=IOS ) ISTRN_CHR(I)
                  CALL FERR ( INT2(IOS), "Writing residual file", INT2(0), &
     &                 INT2(0) )
               ENDDO
               CALL USE_RESNDX ( 'C', FNAMEA )
               FNAMEB = RESOUTFILE(:il)//'.bin'
               CALL BIN_OPEN ( FNAMEB, FILDES2, 'O' )
               IOS = FC_LSEEK ( FILDES2, 0, 2 )
          ENDIF
!
        OBCOUNTR = 0
        CALL OBSTM ( JD_NOM_FIRST, JD_NOM_LAST )
        IF ( SOLVE_DEBUG(1:2) == '48' ) THEN
             IF ( ILEN(SOLVE_DIR_DEBUG) == 0 ) SOLVE_DIR_DEBUG = '/tmp'
!            
             LUN_DEBUG = GET_UNIT()
             IF ( CDBNAM(IDB)(1:1) == '$' ) THEN
                  FINAM_DEBUG = SOLVE_DIR_DEBUG(1:I_LEN(SOLVE_DIR_DEBUG))//'/'// &
     &                          CDBNAM(IDB)(2:ILEN(CDBNAM(IDB)))//'.del'
                ELSE
                  FINAM_DEBUG = SOLVE_DIR_DEBUG(1:I_LEN(SOLVE_DIR_DEBUG))//'/'// &
     &                          CDBNAM(IDB)(1:ILEN(CDBNAM(IDB)))//'.del'
             END IF
             IF ( SOLVE_DEBUG(1:8) == 'GET_IONO' ) THEN
                  FINAM_DEBUG = SOLVE_DIR_DEBUG(1:I_LEN(SOLVE_DIR_DEBUG))//'/'// &
     &                          'iono.dat'
                  OPEN ( UNIT=LUN_DEBUG, FILE=FINAM_DEBUG, STATUS='UNKNOWN', &
     &                   ACCESS='APPEND', IOSTAT=IER )
             END IF
!
             OPEN ( UNIT=LUN_DEBUG, FILE=FINAM_DEBUG, STATUS='UNKNOWN', &
     &              IOSTAT=IER )
             IF ( IER .NE. 0 ) THEN
                  CALL CLRCH  ( STR )
                  CALL INCH   ( IER, STR )
                  CALL ERR_LOG ( 8912, IUER, 'SECND', 'Environment variable '// &
     &                'SOLVE_DEBUG is set. Failure to open '// &
     &                'output debuggin file '// &
     &                 FINAM_DEBUG(1:I_LEN(FINAM_DEBUG))//' -- error '//STR )
                  RETURN
             END IF
!
             WRITE ( LUN_DEBUG, '(A)' ) '# Solve Delay Output.  Format version of 2007.02.02'
             WRITE ( LUN_DEBUG, '(A)' ) '#  '
             WRITE ( LUN_DEBUG, '(A,I3)' ) '# Experiment: '//CDBNAM(IDB)//' Version ', IDBV(IDB)
             WRITE ( LUN_DEBUG, '(A)' ) '#  '
             WRITE ( LUN_DEBUG, '(A)' ) '# Generated on '//GET_CDATE()
             WRITE ( LUN_DEBUG, '(A)' ) '#  '
        END IF
!
        CALL CLRCH ( VTD_CONF_USE )
        IF ( FL_VTD_SES ) THEN
             VTD_CONF_USE = VTD_CONF_SES
           ELSE
             IF ( FL_VTD_GLB ) THEN
                  VTD_CONF_USE = VTD_CONF_GLB
             END IF
        END IF
!
        CALL GETENVAR ( 'VTD_CONF', STR )
        IF ( .NOT. KBATCH ) THEN
              VTD_CONF_USE = STR
        END IF
!
        CALL GETENVAR ( 'TPD_USE', STR )
        IF ( STR .EQ. 'DEBUG' ) THEN
             FL_TPD_DEBUG = .TRUE.
           ELSE IF ( STR .EQ. 'YES' ) THEN
             FL_TPD_DEBUG = .FALSE.
           ELSE
             TPD_FLAG = TPD__UNDF
        END IF
        TPD_USE_FLAG = TPD_FLAG
!
        IF ( TPD_USE_FLAG == TPD__USE  .OR.  TPD_USE_FLAG == TPD__UPD ) THEN
             CALL ERR_PASS ( IUER, IER )
             CALL TPD_INIT ( TPD, DBNAME_CH, VTD_CONF_USE, IDEND-IDBGN+1, &
     &                       INT4(NUMSTA), INT4(NUMSTR), IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8913, IUER, 'SECND', 'Failure during '// &
     &                'attempt to initialize TPD data strucuture while '// &
     &                'experiment '//B3DOBJ%DBNAME_MES//' was beging '// &
     &                'processing' )
                  RETURN
             END IF
!
             CALL ERR_PASS ( IUER, IER )
             CALL TPD_READ ( TPD, TPD_USE_FLAG, VTD_CONF_USE, DBNAME_CH, &
     &                       N_TPD_INIT, TPD_INIT_LIST, FL_TPD_DEBUG, &
     &                       FL_TPD_READ, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8914, IUER, 'SECND', 'Failure during '// &
     &                'attempt to read TPD data strucuture while '// &
     &                'experiment '//B3DOBJ%DBNAME_MES//' was beging '// &
     &                'processing' )
                  RETURN
             END IF
           ELSE 
             FL_TPD_READ = .FALSE.
        END IF
!
        IF ( .NOT. FL_TPD_READ .AND. &
     &       ( TRP_USE == REQ__TRP  .OR.  TRP_USE == USE__TRP ) ) THEN
             CALL ERR_PASS ( IUER, IER )
             CALL LOAD_TRP ( TRP_USE, TRP_DIR, DBNAME_CH, &
     &                       N_FIL_TRP, %VAL(ADR_TRP_FIL_BUF), STS_TRP_FIL, &
     &                       %VAL(ADR_TRP_SES_BUF), STS_TRP_SES, &
     &                       %VAL(ADR_TRP), STS_TRP, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8915, IUER, 'SECND', 'Failure in an attempt '// &
     &                'to load external tropospheric path delay file '// &
     &                'for database '//DBNAME_CH )
                  RETURN
             END IF
        END IF
!
        IF ( ILEN(VTD_CONF_USE) > 0  .AND. .NOT. FL_TPD_READ ) THEN
             IF ( VTD_ADR == 0 ) THEN
                  SIZEOF_VTD = SIZEOF(VTD_PTR)
                  CALL ERR_PASS ( IUER, IER )
                  CALL GRAB_MEM ( IER, MEM_LEN_VTD, MEM_ADR_VTD, 1, &
     &                            SIZEOF_VTD, VTD_ADR )
                  IF ( IER .NE. 0 ) THEN
                       CALL CLRCH ( STR )
                       CALL IINCH ( SIZEOF_VTD, STR )
                       CALL ERR_LOG ( 8916, IUER, 'SECND', 'Failure to '// &
     &                     'allocate '//STR(1:I_LEN(STR))// &
     &                     ' bytes of dynamic memory for VTD' )
                       RETURN
                  END IF
!
                  VTD_STATUS = VTD__ALLC
                  CALL ERR_PASS ( IUER, IER )
                  CALL VTD_INIT ( %VAL(VTD_ADR), IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 8917, IUER, 'SECND', 'Failure to '// &
     &                     'initialize vtd object' )
                       RETURN
                  END IF
                  VTD_STATUS = VTD__INIT
                ELSE
                  MEM_LEN_VTD = 0
             END IF
!
             CALL ERR_PASS ( IUER, IER )
             CALL PRE_VTD  ( %VAL(VTD_ADR), VTD_STATUS, VTD_CONF_USE, &
     &                       FL_VTD_IONO, VTD_IONO_SCALE, IER )
             IF ( IER .NE. 0 ) THEN
                  IF ( SOLVE_DEBUG .NE. 'GET_IONO' ) THEN
                       CALL ERR_LOG ( 8918, IUER, 'SECND', 'Failure during '// &
     &                     'attempt to executer pre-vtd routines '// &
     &                     'while database '//B3DOBJ%DBNAME_MES//' was processing' )
                       RETURN
                     ELSE
                       CALL CLRCH ( SOLVE_DEBUG )
                  END IF
             END IF
        END IF
!
      IF ( IDB .EQ. 1 .AND. &
     &     TRAIN      .AND. &
     &     ( FAST_MODE .EQ. F__B3D  .OR.  FAST_MODE .EQ. F__B1B3D ) ) THEN
!
           CALL CLRCH ( FINAM_FAST )
           FINAM_FAST = PRE_SCR_DIR(1:PRE_SD_LEN)//'FAST'//PRE_LETRS
!
! -------- Reading fields to B3DOBJ objects which contains correcrted
! -------- theoretical delay and rate
!
           CALL ERR_PASS ( IUER, IER )
           CALL RDNOR_B3D ( FINAM_FAST, B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8919, IUER, 'SECND', &
     &              'Error during reading file '// &
     &               FINAM_FAST(1:I_LEN(FINAM_FAST))//' with temporary '// &
     &              'data structure for B3D algorithm while database '// &
     &               CDBNAM(IDB)//' was processing' )
                RETURN
           ENDIF
!
! -------- Extract nominal and actual duration of the session
!
           JD_DUR_ACT = B3DOBJ%JD_ACT_LAST - B3DOBJ%JD_ACT_FIRST
           JD_DUR_NOM = B3DOBJ%JD_NOM_LAST - B3DOBJ%JD_NOM_FIRST
         ELSE
!
! -------- Non-fast mode or no TRAIN mode
!
           JD_DUR_ACT = -1.D0
           CALL OBSTM ( B3DOBJ%JD_NOM_FIRST, B3DOBJ%JD_NOM_LAST )
           JD_DUR_NOM = B3DOBJ%JD_NOM_LAST - B3DOBJ%JD_NOM_FIRST
      END IF
      B3DOBJ%N_PAR = NPARAM ! Total number of parameters
!
! --- Formatting the string with database name
!
      CALL CLRCH ( STR )
      CALL LIB$MOVC3 ( 10, LDBNAM, STR  )
      IVER = INT4 ( IDBV(1) )
      STR(12:) = '<'
      CALL INCH ( IVER, STR(13:) )
      STR( I_LEN(STR)+1: ) = '>'
!
      IF ( FAST_DBG .EQ. F__APP  .OR.   FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, 210 )  str(1:16), fast_mode, fast_dbg
 210       format ( 1X,' CRES:   session ',a,'  fast_mode=',i4, &
     &                 ' fast_dbg=',i4 )
      END IF
!C
      CHI_REDUCE = 0.
!
! --- Initialization of internal data structures used by FLYBY_MAP
!
      CALL FLYBY_MAP_INIT()
!
      STATUS_HFE = HFE__NONE ! Disable high frequency EOP interpolation
      CALL F__CLR_IND ( 3, F__NONE, PLACE, B3DOBJ, B1B3DOBJ )
      IF ( APRIORI_ZENDEL ) THEN
!
! -------- Open file for writing a priori atmosphere path delay for each station
! -------- for each moment.
!
           FATFIL = PRE_SCR_DIR(1:PRE_SD_LEN)//'ATZE'//PRE_LETRS
           LUN_AZ = GET_UNIT()
           OPEN ( UNIT=LUN_AZ, FILE=FATFIL, STATUS='UNKNOWN', IOSTAT=I61 )
           WRITE ( LUN_AZ, '(A,A,A)' ) STR, ' Current date: ', GET_CDATE()
!
           CALL ERR_PASS ( IUER, IER )
           CALL CALS_R  ( INT2(1), 0, 0, CALS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8920, IUER, 'SECND', 'Error in attempt '// &
     &              'to read NAMF-block' )
                RETURN
           END IF
!
           DO 410 J1=1,CALS%L_SCAL
              DO 420 J2=1,CALS%L_STA
                 IF ( CALS%SCAL(J1) .EQ. 'Nieltdry'  .AND. &
     &                CALS%SCAL_APL(J1,J2)                  ) THEN
                      NTD_FL(J2)  = .TRUE.
                      NTD_IND(J2) = J1
                      IF ( G_WARNING ) THEN
                           WRITE (  6, '(A)' ) 'WARNING: calibration '// &
     &                             CALS%SCAL(J1)//' was ON for '// &
     &                             CALS%STANAM(J2)//' in '//STR(1:I_LEN(STR))
                           WRITE (  6, '(A)' ) 'WARNING: spool file has a '// &
     &                                         'WRONG value for zenith path '// &
     &                                         'delay'
                           WRITE ( 23, '(A)' ) 'WARNING: calibration '// &
     &                             CALS%SCAL(J1)//' was ON for '// &
     &                             CALS%STANAM(J2)//' in '//STR(1:I_LEN(STR))// &
     &                             ' -- spool file has a WRONG value for '// &
     &                             'zenith path delay'
                      END IF
                    ELSE IF ( VTD_STATUS .NE. VTD__UNDF ) THEN
                      NTD_FL(J2)  = .TRUE.
                      NTD_IND(J2) = J1
                    ELSE
                      NTD_FL(J2)  = .FALSE.
                      NTD_IND(J2) = 0
                 END IF
!
                 IF ( CALS%SCAL(J1) .EQ. 'Nieltwet'  .AND. &
     &                CALS%SCAL_APL(J1,J2)                  ) THEN
                      NTW_FL(J2)  = .TRUE.
                      NTW_IND(J2) = J1
                      IF ( G_WARNING ) THEN
                           WRITE (  6, '(A)' ) 'WARNING: calibration '// &
     &                             CALS%SCAL(J1)//' was ON for '// &
     &                             CALS%STANAM(J2)//' in '//STR(1:I_LEN(STR))
                           WRITE (  6, '(A)' ) 'WARNING: spool file has a '// &
     &                                         'WRONG value for zenith path '// &
     &                                         'delay'
                           WRITE ( 23, '(A)' ) 'WARNING: calibration '// &
     &                             CALS%SCAL(J1)//' was ON for '// &
     &                             CALS%STANAM(J2)//' in '//STR(1:I_LEN(STR))// &
     &                             ' -- spool file has a WRONG value for '// &
     &                             'zenith path delay'
                      END IF
                    ELSE
                      NTW_FL(J2)  = .FALSE.
                      NTW_IND(J2) = 0
                 END IF
 420          CONTINUE
 410       CONTINUE
      END IF
!
! --- Initilialzation
!
      WW_ALL = 0.0
      JDATE_ALL_BEG = -BIG_VALUE
      JDATE_ALL_MID = 0.0D0
      DO 430 J3=1,NUMSTA
         WW_STA(J3) = 0.0D0
         JDATE_STA_BEG(J3) = -BIG_VALUE
         JDATE_STA_MID(J3) = 0.0D0
 430  CONTINUE
!
! --- Determine whether or not necassary to take into account rates in normal
! --- equations
!
      IF ( DATYP_INQ ( IDATYP, RATE__DTP ) ) THEN
           PLACE%STATUS = F__RAT
         ELSE
           PLACE%STATUS = F__DEL
      END IF
!
      IF ( KSCREEN ) CALL GETXY_MN ( IXX, IYY )
      DO ISTA = 1, NUMSTA
         C_STA(ISTA) = ISITN_CHR(ISTA)
         CALL VTD_NAME_REPAIR ( C_STA(ISTA) )
      END DO
      SKIP_COUNT = 0
!
      IF ( EDC_USE == EDC__REQ .OR. &
     &     EDC_USE == EDC__USE      ) THEN
           IF ( EDC_USE == EDC__REQ ) CALL ERR_PASS ( IUER, IER )
           IF ( EDC_USE == EDC__USE ) CALL ERR_PASS ( 0,    IER )
!
! -------- Read external decimation file
!
           CALL EDC_READ ( DBNAME_CH, EDC_DIR, EDC, IER )
           IF ( IER .NE. 0  .AND. EDC_USE == EDC__REQ ) THEN
                CALL ERR_LOG ( 8921, IUER, 'SECND', 'Error in an attempt '// &
     &              'to open external decimation file' )
                RETURN
           END IF
!
           WRITE ( 23, '(A)' ) ' Decimation for database '//DBNAME_CH// &
     &                         ' Procedure '//EDC%HEA%PRC_NAME(1:I_LEN(EDC%HEA%PRC_NAME))
      END IF
!
! --- Initialization of the atmosphere counter
!
      NAT = 0
      TZD_AVG = 0.0D0
      TZD_RMS = 0.0D0
      WZD_AVG = 0.0D0
      WZD_RMS = 0.0D0
      STM_AVG = 0.0D0
      STM_RMS = 0.0D0
      SPR_AVG = 0.0D0
      SPR_RMS = 0.0D0
      TIM_ATM_BEG = 0.0D0
      TIM_ATM_END = 0.0D0
      LAST_HUM_TIME = 1.D9 ! Initialization
!
      IF ( ILEN(AOC_FIL) > 0 ) THEN
           INQUIRE ( FILE=AOC_FIL, EXIST=FL_AOC ) 
           IF ( .NOT. FL_AOC ) THEN
                CALL ERR_LOG ( 8922, IUER, 'SECND', 'Apriori observation '// &
     &              'correction file '//TRIM(AOC_FIL)//' does not exist' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT ( AOC_FIL, MAX_OBS, BUF_AOC, NA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8923, IUER, 'SECND', 'Error in an attempt '// &
     &              'to read the external apriori observation correction '// &
     &              'file '//EDIT_FIL )
                RETURN
           END IF
!
           IF ( BUF_AOC(1)(1:LEN(LABEL__AOC)) .NE.LABEL__AOC ) THEN
                CALL ERR_LOG ( 8924, IUER, 'SECND', 'Wrong 1st line of the '// &
     &              'external a priori observation correction file '//TRIM(AOC_FIL)// &
     &              ' -- '//TRIM(BUF_AOC(1))//' while '//LABEL__AOC// &
     &              ' was expected'  )
                RETURN
           END IF
           IF ( BUF_AOC(3)(15:24) .NE. DBNAME_CH(1:10) ) THEN
                CALL ERR_LOG ( 8925, IUER, 'SECND', 'Experiment name '// &
     &              'defined in external a priori observation correction file '// &
     &              TRIM(AOC_FIL)//' -- '//BUF_AOC(3)(15:24)//' does not match to the '// &
     &              'experiment being processed '//DBNAME_CH )
                RETURN
           END IF
!
           AOC_DEL = 0.0D0
           DO 440 J4=1,NA
              IF ( BUF_AOC(J4)(1:1)   == '#' ) GOTO 440
              IF ( ILEN(BUF_AOC(J4))  ==  0  ) GOTO 440
              IF ( BUF_AOC(J4)(1:8)   == 'Ind_obs:' .AND. &
     &             BUF_AOC(J4)(17:24) == 'Add_del:'       ) THEN
                   CONTINUE 
                 ELSE
                   CALL CLRCH ( STR )
                   CALL INCH  ( J4, STR )
                   CALL ERR_LOG ( 8926, IUER, 'SECND', 'Error in parsing '// &
     &                 'the '//TRIM(STR)//' line the external apriori '// &
     &                 'observation correction  file '//AOC_FIL )
                   RETURN
              END IF
              CALL CHIN ( BUF_AOC(J4)(10:15), IND_OBS )
              READ ( UNIT=BUF_AOC(J4)(26:38), FMT='(F13.5)', IOSTAT=IER ) AOC_DEL(IND_OBS)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J4, STR )
                   CALL ERR_LOG ( 8927, IUER, 'SECND', 'Error in parsing '// &
     &                 'the value in the '//TRIM(STR)//' line of the external '// &
     &                 'apriori observation correction file '//AOC_FIL )
                   RETURN
              END IF
 440       CONTINUE 
         ELSE 
           FL_AOC = .FALSE.
      END IF
!
      IF ( ILEN(EDIT_FIL) > 0 ) THEN
           FL_EDIT = .TRUE.
           FL_SUP  = .FALSE.
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( EDIT_FIL, MAX_OBS, BUF_EDIT, NE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8928, IUER, 'SECND', 'Error in an attempt '// &
     &              'to read external edit flag file' )
                RETURN
           END IF
           DO 450 J5=1,NE
              IF ( BUF_EDIT(J5)(1:1) == '#' ) GOTO 450
              IF ( ILEN(BUF_EDIT(J5)) == 0  ) GOTO 450
              CALL CHIN ( BUF_EDIT(J5), IND_OBS )
              IF ( IND_OBS > 0 .AND. IND_OBS .LE. MAX_OBS ) THEN
                   FL_SUP(IND_OBS) = .TRUE.
              END IF
 450       CONTINUE 
        ELSE 
           FL_EDIT = .FALSE.
           FL_SUP  = .FALSE.
      END IF
!
      IF ( ILEN(ADDW_FIL) > 0 ) THEN
           FL_ADDW = .TRUE.
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( ADDW_FIL, MAX_OBS, BUF_ADDW, N_WEI, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8929, IUER, 'SECND', 'Error in an attempt '// &
     &              'to read external additive flag file '//ADDW_FIL )
                RETURN
           END IF
!
           IF ( BUF_ADDW(1)(1:LEN(LABEL__ADDW)) .NE. LABEL__ADDW .AND. &
     &          BUF_ADDW(1)(1:LEN(LABEL__ADDW)) .NE. LABEL__ADDW_V1    ) THEN
                CALL ERR_LOG ( 8930, IUER, 'SECND', 'Wrong 1st line of the '// &
     &              'external additive flag file '//TRIM(ADDW_FIL)// &
     &              ' -- '//TRIM(BUF_ADDW(1))//' while '//LABEL__ADDW// &
     &              ' was expected'  )
                RETURN
           END IF
           IF ( BUF_ADDW(3)(15:24) .NE. DBNAME_CH(1:10) ) THEN
                CALL ERR_LOG ( 8931, IUER, 'SECND', 'Experiment name '// &
     &              'defined in external additive flag file '//TRIM(ADDW_FIL)// &
     &              ' -- '//BUF_ADDW(3)(15:24)//' does not match to the '// &
     &              'experiment being processed '//DBNAME_CH )
                RETURN
           END IF
           ADDW_EXT = 0.0D0
           FL_ADDW_IONO = .FALSE.
           FL_ADDW_BW   = .FALSE.
           DO 460 J6=1,N_WEI
              IF ( BUF_ADDW(J6)(1:29) == '# Baseline weights: included'    ) FL_ADDW_BW = .TRUE.
              IF ( BUF_ADDW(J6)(1:31) == '# Baseline weights: independent' ) FL_ADDW_BW = .FALSE.
              IF ( BUF_ADDW(J6)(1:1) == '#' ) GOTO 460
              IF ( ILEN(BUF_ADDW(J6)) == 0  ) GOTO 460
              CALL CHIN ( BUF_ADDW(J6)(10:15), IND_OBS )
              IF ( BUF_ADDW(J6)(1:32) == '# Average Ionospheric frequency:' ) THEN
                   READ ( UNIT=BUF_ADDW(J6)(34:46), FMT='(F13.5)' ) ADDW_FRQ
                   FL_ADDW_IONO = .TRUE.
              END IF
              IF ( IND_OBS > 0 .AND. IND_OBS .LE. MAX_OBS ) THEN
                   READ ( UNIT=BUF_ADDW(J6)(27:38), FMT='(D12.5)' ) ADDW_EXT(IND_OBS)
              END IF
  460      CONTINUE 
        ELSE 
           FL_ADDW = .FALSE.
           ADDW_EXT = 0.0D0
      END IF
!
      IF ( ILEN(DTEC_FIL) > 0 ) THEN
           FL_DTEC = .TRUE.
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( DTEC_FIL, MAX_OBS, BUF_DTEC, N_DTEC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8932, IUER, 'SECND', 'Error in an attempt '// &
     &              'to read external additive flag file '//DTEC_FIL )
                RETURN
           END IF
!
           IF ( BUF_DTEC(1)(1:LEN(LABEL__DTEC)) .NE. LABEL__DTEC ) THEN
                CALL ERR_LOG ( 8933, IUER, 'SECND', 'Wrong 1st line of the '// &
     &              'external dTEC flag file '//TRIM(DTEC_FIL)// &
     &              ' -- '//TRIM(BUF_DTEC(1))//' while '//LABEL__DTEC// &
     &              ' was expected'  )
                RETURN
           END IF
           IF ( BUF_DTEC(8)(15:24) .NE. DBNAME_CH(1:10) ) THEN
                CALL ERR_LOG ( 8934, IUER, 'SECND', 'Experiment name '// &
     &              'defined in external dTEC flag file '//TRIM(DTEC_FIL)// &
     &              ' -- '//BUF_DTEC(8)(15:24)//' does not match to the '// &
     &              'experiment being processed '//DBNAME_CH )
                RETURN
           END IF
!
           DTEC_GPS_EXT    = 0.0D0
           DTEC_ADJ_EXT    = 0.0D0
           DTEC_ERR_EXT    = 0.0D0
           DEL_BIAS_UL_EXT = 0.0D0
           DO 470 J7=1,N_DTEC
              IF ( BUF_DTEC(J7)(1:1) == '#' ) GOTO 470
              IF ( ILEN(BUF_DTEC(J7)) == 0  ) GOTO 470
              CALL CHIN ( BUF_DTEC(J7)(10:15), IND_OBS )
              IF ( IND_OBS > 0 .AND. IND_OBS .LE. MAX_OBS ) THEN
                     READ ( UNIT=BUF_DTEC(J7)(28:37),  FMT='(D10.2)' ) DTEC_GPS_EXT(IND_OBS)
                     READ ( UNIT=BUF_DTEC(J7)(50:59),  FMT='(D10.2)' ) DTEC_ADJ_EXT(IND_OBS)
                     READ ( UNIT=BUF_DTEC(J7)(72:81),  FMT='(D10.2)' ) DTEC_ERR_EXT(IND_OBS)
                     READ ( UNIT=BUF_DTEC(J7)(94:106), FMT='(D13.6)' ) DEL_BIAS_UL_EXT(IND_OBS)
              END IF
  470      CONTINUE 
        ELSE 
           FL_DTEC = .FALSE.
           DTEC_GPS_EXT    = 0.0D0
           DTEC_ADJ_EXT    = 0.0D0
           DTEC_ERR_EXT    = 0.0D0
           DEL_BIAS_UL_EXT = 0.0D0
      END IF
!
      IF ( ILEN(EXT_ERR_FIL) > 0 ) THEN
           FL_EXT_ERR = .TRUE.
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( EXT_ERR_FIL, MAX_OBS, BUF_EXT_ERR, N_EXT_ERR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8935, IUER, 'SECND', 'Error in an attempt '// &
     &              'to read external errir flag file '//EXT_ERR_FIL )
                RETURN
           END IF
!
           IF ( BUF_EXT_ERR(1)(1:LEN(LABEL__EXT_ERR)) .NE. LABEL__EXT_ERR ) THEN
                CALL ERR_LOG ( 8936, IUER, 'SECND', 'Wrong 1st line of the '// &
     &              'external error file file '//TRIM(EXT_ERR_FIL)// &
     &              ' -- '//TRIM(BUF_EXT_ERR(1))//' while '//LABEL__EXT_ERR// &
     &              ' was expected'  )
                RETURN
           END IF
           IF ( BUF_EXT_ERR(3)(15:24) .NE. DBNAME_CH(1:10) ) THEN
                CALL ERR_LOG ( 8937, IUER, 'SECND', 'Experiment name '// &
     &              'defined in external external error file '//TRIM(EXT_ERR_FIL)// &
     &              ' -- '//BUF_EXT_ERR(3)(15:24)//' does not match to the '// &
     &              'experiment being processed '//DBNAME_CH )
                RETURN
           END IF
           EXT_ERR = 0.0D0
           DO 480 J8=1,N_EXT_ERR
              IF (      BUF_EXT_ERR(J8)(1:1) == '#' ) GOTO 480
              IF ( ILEN(BUF_EXT_ERR(J8))     ==  0  ) GOTO 480
              CALL CHIN ( BUF_EXT_ERR(J8)(10:15), IND_OBS )
              IF ( IND_OBS > 0 .AND. IND_OBS .LE. MAX_OBS ) THEN
                   READ ( UNIT=BUF_EXT_ERR(J8)(26:38), FMT='(F13.6)' ) EXT_ERR(IND_OBS)
              END IF
 480       CONTINUE 
         ELSE
           FL_EXT_ERR = .FALSE.
      END IF
      CALL OBSTM ( FJDOBS, LJDOBS )
      CALL JD_TO_MJD_SEC ( FJDOBS, MJD_1ST, UTC_1ST )
!
! === Start main loop over the observations
!     =====================================
!
      DO 1000 NOBS = IDBGN,IDEND
         KOBS = NOBS - IDBGN + 1  ! Number of the observation counted form 1
         IOBS   = NOBS
         IDLT   = '>'
!
         IF ( K_MN  .AND.  NOBS .EQ. IDBGN ) THEN
              WRITE  ( LBUF(IPTR), 9001 ) CDBNAM(IDB), IDEND - IDBGN + 1
 9001         FORMAT ( " Residuals from data base ",A, &
     &                 " Obs          (",i8,") " )
              CALL SETCR_MN ( 0, IYY  )
              CALL ADDSTR_F ( LBUF(IPTR) (:PAGEWID) )
              CALL REFRESH_MN()
         ENDIF
         IF ( K_MN .AND. ( MOD( KOBS, 5000) .EQ. 0  .OR. NOBS .EQ. IDEND ) ) THEN
              WRITE ( BUF, '(I8)' ) KOBS
              CALL SETCR_MN ( 41, IYY )
              CALL ADDSTR_F ( BUF(1:8) )
              CALL SETCR_MN ( 72, IYY )
              CALL REFRESH_MN()
         ENDIF
!
! ------ Create the datebase-dependent random number seed
!
         CALL CHIN ( DBNAME_CH(1:8), RAN_SEED_I4 )
         IF ( RAN_SEED_I4 < 0 ) RAN_SEED_I4 = 0
         IF ( SEED_INIT_I4 > -1000000000 ) THEN
              RAN_SEED_I4 = RAN_SEED_I4 - SEED_INIT_I4 
            ELSE 
!
! ----------- ... in order to avoid integer overflow
!
              RAN_SEED_I4 = RAN_SEED_I4 + SEED_INIT_I4 
         END IF
!
         IF ( ILEN(TEC_NOISE_FILE) > 0 ) THEN
              CALL ERR_PASS  ( IUER, IER ) 
              CALL PARSE_TCN ( TEC_NOISE_FILE, M__TCN, TCN, L_TCN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8935, IUER, 'SECND', 'Error in '// &
     &                 'an attempt to parse GPS TEC noise file '// &
     &                  TEC_NOISE_FILE )
                   RETURN
              END IF
            ELSE 
              L_TCN = 0
         END IF
!
! ------ Read the next record in the 'OBSFIL'
!
         CALL USE_OBSFIL ( IOBSFIL, IOBS, 'R' )
         CALL JD_TO_MJD_SEC  ( FJD, MJD_UTC_OBS, UTC_OBS )
         UTC_OBS = UTC_OBS + FRACT*86400.0D0
!
         DER_DEL = 0.0D0
         IF ( ILEN(VTD_CONF_USE) > 0  .AND.  .NOT. FL_TPD_READ ) THEN
!
! ----------- Load meteorological parameters of the first station
! ----------- into the VTD record
!
              PRES_VAL = ATMPR(1)*100.0D0
              TEMP_VAL = TEMPC(1) + 273.16D0
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_METEO_IN ( %VAL(VTD_ADR), C_STA(ISITE(1)), &
     &                            PRES_VAL, TEMP_VAL, TEMP_VAL, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8936, IUER, 'SECND', 'Error in an '// &
     &                 'attempt to load meteorological parameters '// &
     &                 'for station '//C_STA(ISITE(1)) )
                   RETURN
              END IF
!
! ----------- Load meteorologial parameters for the second station
!
              PRES_VAL = ATMPR(2)*100.0D0
              TEMP_VAL = TEMPC(2) + 273.16D0
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_METEO_IN ( %VAL(VTD_ADR), C_STA(ISITE(2)), &
     &                            PRES_VAL, TEMP_VAL, TEMP_VAL, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8937, IUER, 'SECND', 'Error in an '// &
     &                 'attempt to load meteorological parameters '// &
     &                 'for station '//C_STA(ISITE(2)) )
                   RETURN
              END IF
!
              CALL JD_TO_MJD_SEC  ( FJD, MJD_UTC_OBS, UTC_OBS )
              UTC_OBS = UTC_OBS + FRACT*86400.0D0
              IF ( NOBS == 1 ) THEN
!
! ---------------- This trick is done since VLBI formatter stores pseudo-UTC.
! ---------------- We need to record UTC-TAI(t) at the beginning of the
! ---------------- experiment and apply it to all observations, regardless
! ---------------- whether the new clock jump took place during the experiment
!
                   CALL VTD_UTC_TO_TAI ( %VAL(VTD_ADR), MJD_UTC_OBS, &
     &                                   UTC_OBS, TAI_OBS, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8938, IUER, 'SECND', 'Error in an '// &
     &                      'attempt to get UTC-minus TAI' )
                        RETURN
                   END IF
                   UTC_MINUS_TAI = UTC_OBS - TAI_OBS
                   IF ( UTC_MINUS_TAI >  43200.0D0 ) UTC_MINUS_TAI = UTC_MINUS_TAI - 86400.0D0
                   IF ( UTC_MINUS_TAI < -43200.0D0 ) UTC_MINUS_TAI = UTC_MINUS_TAI + 86400.0D0
              END IF
!
              UTC_M_TAI = UTC_MINUS_TAI
              TAI_OBS = UTC_OBS - UTC_M_TAI
              IF ( TAI_OBS < 0.0D0 ) THEN
                   TAI_OBS = TAI_OBS + 86400.0D0
                   MJD_TAI_OBS = MJD_UTC_OBS - 1
                 ELSE
                   MJD_TAI_OBS = MJD_UTC_OBS
              END IF
!
! ----------- Set fields of OBS_TYP
!
              IF ( SOLVE_DEBUG(1:8) == 'PUT_IONO' ) THEN
                   IDATYP_SAVE = IDATYP
                   IDATYP = GX__DTP
              END IF
!
              CALL SET_OBSTYP ( OBS_TYP )
              IF ( SOLVE_DEBUG(1:8) == 'PUT_IONO' ) THEN
                   IDATYP = IDATYP_SAVE 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_DELAY ( ISTRN_CHR(ISTAR), C_STA(ISITE(1)), &
     &                         C_STA(ISITE(2)), MJD_TAI_OBS, TAI_OBS, &
     &                         OBS_TYP, %VAL(VTD_ADR), DELAY_THR, &
     &                         RATE_THR, DER_DEL, DER_RAT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8939, IUER, 'SECND', 'Error in an '// &
     &                 'attempt to compute VLBI time delay' )
                   RETURN
              END IF
              IONO_GPS = DER_DEL(VTD__IONO2) - DER_DEL(VTD__IONO1)
              IF ( SOLVE_DEBUG(1:8) == 'PUT_IONO' ) THEN
                   DELAY_THR = DELAY_THR - IONO_GPS
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL POST_VTD ( NOBS, %VAL(VTD_ADR), TRP_USE, %VAL(ADR_TRP), &
     &                        STS_TRP, DELAY_THR, RATE_THR, DER_DEL, &
     &                        DER_RAT, FL_NO_IONO_DEL, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8940, IUER, 'SECND', 'Error in an '// &
     &                 'attempt to collect results of VTD' )
                   RETURN
              END IF
            ELSE ! IF ( .NOT. FL_TPD_READ ) THEN
              IF ( NOBS == 1 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL GET_UTC_M_TAI ( NERS, MJD_UTC_OBS, UTC_OBS, UTC_M_TAI, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8941, IUER, 'SECND', 'Error in '// &
     &                      'an attempt to compute function UTC-minus-TAI' )
                        RETURN
                   END IF
                   UTC_M_TAI_SAVE = UTC_M_TAI
                 ELSE
                   UTC_M_TAI = UTC_M_TAI_SAVE
              END IF
              TAI_OBS = UTC_OBS - UTC_M_TAI
              IF ( TAI_OBS < 0.0D0 ) THEN
                   TAI_OBS = TAI_OBS + 86400.0D0
                   MJD_TAI_OBS= MJD_UTC_OBS - 1
                 ELSE
                   MJD_TAI_OBS= MJD_UTC_OBS
              END IF
         END IF
         IF ( NOBS == 1 ) THEN
              MJD_TAI_OBS_BEG = MJD_TAI_OBS 
              TAI_OBS_BEG     = TAI_OBS  
         END IF
!
         IF ( FL_TPD_READ ) THEN
              IF ( NOBS == 1 ) THEN
                   CALL USE_COMMON ( 'OR' )
                   CALL USE_GLBFIL ( 'OR' )
                   CALL USE_PARFIL ( 'OR' )
                   CALL SOCOM_EXT()
                   IDBEND = IDBEND_SAVE
                   IF ( KBIT(IDBSEL,IDB) ) CALL SBIT ( IDBEST, IDB, INT2(1) )
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL TPD_GET  ( TPD, NOBS, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8942, IUER, 'SECND', 'Failure during '// &
     &                 'attempt to get TPD data structure while '// &
     &                 'experiment '//B3DOBJ%DBNAME_MES//' was beging '// &
     &                 'processing' )
                   RETURN
              END IF
!
! ----------- Temporary (2017.10.18)
!
              DER_DEL(VTD__SUR_TEM1) = TEMPC(1) + 273.16
              DER_DEL(VTD__SUR_PRS1) = 100.0*ATMPR(1) 
              DER_DEL(VTD__SUR_TEM2) = TEMPC(2) + 273.16
              DER_DEL(VTD__SUR_PRS2) = 100.0*ATMPR(2) 
!
              IF ( NOBS == 1 ) THEN
!
! ---------------- We do it in order to save 5 varaibles read from the
! ---------------- header of TPD file
!
                   CALL USE_COMMON ( 'WC' )
                   CALL USE_GLBFIL ( 'WC' )
                   CALL USE_PARFIL ( 'WC' )
              END IF
      END IF
!
      IF ( FL_EDIT ) THEN
           IF ( FL_SUP(NOBS) ) THEN
                AUTO_SUP = IBSET ( AUTO_SUP, INT4(EXTS__SPS) )
              ELSE 
                USER_SUP = IBCLR ( USER_SUP, INT4(IDATYP) )
           END IF
      END IF
! 
      IF ( SIMULATION_TEST .AND. ALL_SIM_FLG ) THEN
           IUNW = 0
      ENDIF
      IF ( NOBS == 1 ) THEN
           TMIN = FJD + FRACTC
      END IF
!
! --- Accumulate humidity values for later calculation of average and rms
! --- Also accumulate total zenith path delay, wet zenith path delay.
! --- Also store first and last observation of each station
!
      DO I=1,2
         J = ISITE(I)
         IF ( NHUM(J).EQ.0  .OR.  DABS(LAST_HUM_TIME(J)-FRACT) .GT. 1.D-5 ) THEN
              LAST_HUM_TIME(J) = FRACT
              IF ( AVG_HUMID(J) .GE. 0.0D0 .AND. AVG_HUMID(J) .LE. 100.0D0 ) THEN
                   AVG_HUMID(J) = AVG_HUMID(J) + RELHU(I)*100.D0
                   NHUM(J) = NHUM(J) + 1
              END IF
              IF ( NHUM(J) .GT. 2048 ) THEN
                   CALL FERR( INT2(222), 'More than 2048 met values found', &
     &                  INT2(0), INT2(0) )
                   STOP
              ENDIF
              HUM(NHUM(J),J) = RELHU(I)*100.D0
!
              NAT(J) = NAT(J) + 1
              TZD_AVG(J) = TZD_AVG(J) + ATM_ZENDEL(I)
              TZD_RMS(J) = TZD_RMS(J) + ATM_ZENDEL(I)**2
              WZD_AVG(J) = WZD_AVG(J) + TROP_WZD(I)
              WZD_RMS(J) = WZD_RMS(J) + TROP_WZD(I)**2
!
              IF ( I == 1 ) THEN
                   STM_AVG(J) = STM_AVG(J) +  TEMPC(1) + 273.16 - DER_DEL(VTD__SUR_TEM1)
                   STM_RMS(J) = STM_RMS(J) + (TEMPC(1) + 273.16 - DER_DEL(VTD__SUR_TEM1))**2
                   SPR_AVG(J) = SPR_AVG(J) +  100.0*ATMPR(1) - DER_DEL(VTD__SUR_PRS1)
                   SPR_RMS(J) = SPR_RMS(J) + (100.0*ATMPR(1) - DER_DEL(VTD__SUR_PRS1))**2
                 ELSE 
                   STM_AVG(J) = STM_AVG(J) +  TEMPC(2) + 273.16 - DER_DEL(VTD__SUR_TEM2)
                   STM_RMS(J) = STM_RMS(J) + (TEMPC(2) + 273.16 - DER_DEL(VTD__SUR_TEM2))**2
                   SPR_AVG(J) = SPR_AVG(J) +  100.0*ATMPR(2) - DER_DEL(VTD__SUR_PRS2)
                   SPR_RMS(J) = SPR_RMS(J) + (100.0*ATMPR(2) - DER_DEL(VTD__SUR_PRS2))**2
              END IF
!
              IF ( NAT(J) == 1 ) THEN
                   TIM_ATM_BEG(J) = (MJD_TAI_OBS - MJD_TAI_OBS_BEG)*86400.0D0 + &
     &                              (TAI_OBS - TAI_OBS_BEG)
              ENDIF
              TIM_ATM_END(J) = (MJD_TAI_OBS - MJD_TAI_OBS_BEG)*86400.0D0 + &
     &                         (TAI_OBS - TAI_OBS_BEG)
         ENDIF
      ENDDO
!
! --- Handle ad hoc case of when CALC produces an observation
! --- with an impossible elevation.
!
      IF ( LQUAL_CHR .EQ. ' 0' .AND. &
     &     ( ELEV(1) .LT. 0.D0 .OR.  ELEV(1) .GT. PI__NUM/2.D0  .OR. &
     &     ELEV(2) .LT. 0.D0 .OR.  ELEV(2) .GT. PI__NUM/2.D0 )    ) THEN
!
           ELEV(1) = .001D0
           ELEV(2) = .001D0
           DOBS = 0.D0
           ROBS = 0.D0
      ENDIF
!
! --- Do the flyby mapping of DT and RT
!
      IF ( ( FAST_MODE .EQ. F__B3D   .OR.  FAST_MODE .EQ. F__B1B3D   ) .AND. &
     &       B3DOBJ%FIRST_FIELD .EQ. INT4(IDB)  ) THEN
!
! --------- Extracting corrected DT and RT (theoreticals for delay and rate)
! --------- from fields of B3DOBJ. They were saved in B3DOBJ by PROC
!
            IF ( .NOT. FL_SUBST ) THEN
                 DT = B3DOBJ%DT(KOBS)
                 RT = B3DOBJ%RT(KOBS)
            END IF
         ELSE
!
! -------- Since nothing were kept we make recalculation of flyby corrections
! -------- to theoretical delay and rate
!
! -------- Making flyby calibration: adding to DT (theoretical time delay)
! -------- and to RT (theoretical delay rate) some corrections:
! --------
! -------- 1) station substitution
! -------- 2) source substitution
! -------- 3) precession-nutation substitution (7 terms)
! -------- 4) substitution nutation daily offsets
! -------- 5) UT1/PM substitution
! -------- 6) High-frequency EOP parameters
!
           CALL FLYBY_MAP()
      END IF
!
! --- Preparing data for calibration
!
      TAU_CALC     = DT/1.D6
      RATE_CALC    = RT
      IF ( IS_R8_NAN ( DOBS   ) ) DOBS   = -1.0D0
      IF ( IS_R8_NAN ( DOBSXS ) ) DOBSXS = -1.0D0
      TAUGR_OBS_X  = DOBS/1.D6
      TAUGR_OBS_S  = DOBSXS/1.D6
      TAUPH_OBS_X  = DPH/1.D6
      TAUPH_OBS_S  = DPHXS/1.D6
      TAUSB_OBS_X  = DNB/1.D6
      TAUSB_OBS_S  = DNB_S/1.D6
      TAUGR_ERR_X  = DERR
      TAUGR_ERR_S  = DERRXS
      TAUPH_ERR_X  = DPHER
      TAUPH_ERR_S  = DPHERXS
      TAUSB_ERR_X  = DNBER
      TAUSB_ERR_S  = DNBER_S
      RATE_OBS_X   = ROBS
      RATE_OBS_S   = ROBSXS
      RATE_ERR_X   = RERR
      RATE_ERR_S   = RERRXS
      IF ( .NOT. FL_EQUAL_EFF_FREQ ) THEN
           FREQ_GR_X    = EFFREQ*1.D6
           FREQ_GR_S    = EFFREQ_XS*1.D6
           FREQ_PH_X    = PHEFFREQ*1.D6
           FREQ_PH_S    = PHEFFREQ_XS*1.D6
           FREQ_RATE_X  = REFFREQ*1.D6
           FREQ_RATE_S  = REFFREQ_XS*1.D6
         ELSE
           FREQ_GR_X    = MEAN_EFF_FREQ(1)*1.D6
           FREQ_GR_S    = MEAN_EFF_FREQ(2)*1.D6
           FREQ_PH_X    = MEAN_EFF_FREQ(3)*1.D6
           FREQ_PH_S    = MEAN_EFF_FREQ(4)*1.D6
           FREQ_RATE_X  = MEAN_EFF_FREQ(5)*1.D6
           FREQ_RATE_S  = MEAN_EFF_FREQ(6)*1.D6
      END IF
!
      IF ( IS_R8_NAN ( FREQ_GR_X ) ) THEN
!
! -------- Not a number! O-o-o! Set a bogus value to prevent operations
! -------- with Not-A-Numbers
!
           FREQ_GR_X = 8.2D9
      END IF
      IF ( IS_R8_NAN ( FREQ_GR_S ) ) THEN
!
! -------- Not a number! O-o-o! Set a bogus value to prevent operations
! -------- with Not-A-Numbers
!
           FREQ_GR_S = 2.2D9
      END IF
!
      IF ( IS_R8_NAN ( FREQ_PH_X ) ) THEN
!
! -------- Not a number! O-o-o! Set a bogus value to prevent operations
! -------- with Not-A-Numbers
!
           FREQ_PH_X = 8.2D9
      END IF
      IF ( IS_R8_NAN ( FREQ_PH_S ) ) THEN
!
! -------- Not a number! O-o-o! Set a bogus value to prevent operations
! -------- with Not-A-Numbers
!
           FREQ_PH_S = 2.2D9
      END IF
      IF ( DATYP_INQ ( IDATYP, FUSED__DTP ) .AND. &
     &     FREQ_GR_X < FREQ__SOLVE_MIN  .OR. FREQ_GR_S < FREQ__SOLVE_MIN ) THEN
           FREQ_GR_X    = MEAN_EFF_FREQ(1)*1.D6
           FREQ_GR_S    = MEAN_EFF_FREQ(2)*1.D6
      END IF
!
! --- Fixing pathological case with wrong databases. Normally it should never
! --- occur since sich cases should be tracked by SDBH
!
      IF ( DERR .LT. -1.D-3  .OR.  DERR .GT. 1.D-3 ) THEN
           DERR = 1.D-3
      END IF
!
! --- Saving some variables which will be sploiled by GET_CALIB
!
      DT_SAVE    = DT
      RT_SAVE    = RT
      DERR_SAVE  = DERR
      RERR_SAVE  = RERR
      DPHER_SAVE = DPHER
!
! --- Zeroing some variables. They will be modified (calibrated) by
! --- GET_CALIB. As a result they will accumulate all calibrations imposed
! --- by GET_CALIB. These trick is done to maintain compatibility with
! --- old versions.
!
      DT    = 0.0D0
      RT    = 0.0D0
!
      IF ( INDEX ( '123456789', LQUAL_CHR(2:2)    ) .GT. 0  .OR. &
     &     INDEX ( '123456789', LQUALXS_CHR(2:2)  ) .GT. 0  .OR. &
     &     SUPMET .EQ. SUPMET__PRE91                             ) THEN
!
! -------- Go here if the quality code is larger then zero at
! -------- least at one band. Remind: LQUALXS_CHR = '  ' means merey the
! -------- fact that we don't have information about the quaily code at
! -------- the opposite band.
!
! -------- Remind: NO FRINGE observations are allowed in SUPEMET__PRE91
! -------- mode.
!
!
! -------- Calculation different mapping functions for using them in partials
! -------- on troposphere delay in zenith direction
!
           CALL ATMPART ( ITT, ISITE, ISITN, ISTAR, VSTARC, &
     &                    AZ, ELEV, ATMPR, RELHU, TEMPC, LATS, HEIGHTS, &
     &                    AX_OFFS, AX_TYPES, BARO_CALS, BARO_HEIGHTS, IDB )
!
! -------- Making calibration: adding to DT (theoretical time delay)
! -------- and to RT (thoretical delay rate) some corrections:
!
! -------- 1) observation dependent contributions where requested;
! -------- 2) non-flyby calibrations;
! -------- 3) Apply the selected flyby calibrations:
! -------- 4) Searching over stations and across the calibration bits
! --------    in JCAFFL, and apply the calibrations where requested.
! --------    Signs have been selected in SDBH
! -------- 5) Add troposphere noise based on average atmosphere delay
! --------    (roughly elevation dependent)
! -------- 6) add ionosphere calibration and modify errors;
! -------- 7) setting flag of goodness of the observation due to ionosphere
! --------    status
! -------- 8) Apply reweighting constants
!
           CALL GET_CALIB ( JCAPPL, JSITI, ITT, NOGOOD, ISITE, DT, RT, &
     &                      CALIBS, ICORR, GION, GIONSG, PHION, PHIONS, &
     &                      DERR, RERR, DPHER, ITTB, ET, SE, SS, CALIBB, &
     &                      CALIBM, OBCAPL, MCAPL, ISITN, ISTAR, VSTARC, &
     &                      AZ, ELEV, ATMPR, RELHU, TEMPC, &
     &                      DERR_RAW, RERR_RAW, DPHER_RAW, LATS, HEIGHTS, &
     &                      AX_OFFS, AX_TYPES, BARO_CALS, BARO_HEIGHTS, &
     &                      APP, JCAFFL, NFCAL, FCAL_NAMES, NAMSTA, IDB, &
     &                      EFFREQ, PHEFFREQ, REFFREQ, REFFREQ_XS, EFFREQ_XS, &
     &                      PHEFFREQ_XS, AXDIF, ISTRN_CHR(ISTAR), &
     &                      SOURCE_WEIGHT_FILE, SOURCE_WEIGHTS, AVG_ATM, &
     &                      KELDEP_NOISE, ATM_ZENDEL, &
     &                      RWT_EL_USE, RWT_SRC_USE, TROP_WZD, AP, FJD, FRACTC, &
     &                      %VAL(ADR_TRP), STS_TRP, TRP_USE, VTD_STATUS )
           IF ( APRIORI_ZENDEL ) THEN
                IF ( VTD_STATUS .EQ. VTD__UNDF ) THEN
                     CALL REF_ELL ( 0, VSITEC(1,ISITE(1)), PHI_GCN, &
     &                              PHI_GDT, LAMBDA, H_ELL, RD, G_ACC )
                     ATM_ZENDEL(1) = ZENDEL_SAA ( ATMPR(1)*100.0D0, PHI_GCN, &
     &                                            H_ELL )
                     CALL REF_ELL ( 0, VSITEC(1,ISITE(2)), PHI_GCN, &
     &                              PHI_GDT, LAMBDA, H_ELL, RD, G_ACC )
                END IF
                DO 490 J9=1,2
                   WRITE  ( LUN_AZ, 110 ) ISITN_CHR(ISITE(J9)), FJD + FRACT, &
     &                                ATM_ZENDEL(J9)
 110               FORMAT ( A8, 1X, F16.8, 1X, 1PD14.6 )
 490            CONTINUE
           END IF
!
! -------- Check whether DT and RT are good values
!
           IF ( IS_R8_NAN( DT ) ) THEN
                CALL CLRCH ( STR )
                CALL INCH ( NOBS, STR )
                CALL FERR ( INT2(4441), 'SECND: Observation '// &
     &               STR(1:I_LEN(STR))//' Calibration for delay is not-a-number. Probably, '// &
     &              'database or scratch file are corrupted', INT2(0), INT2(0) )
                STOP 'CRES  Abnormal termination'
           END IF
!
           IF ( .NOT. NORATE_FLAG  .AND.  IS_R8_NAN( RT ) ) THEN
                CALL CLRCH ( STR )
                CALL INCH ( NOBS, STR )
                CALL FERR ( INT2(4442), 'SECND: Observation '// &
     &               STR(1:I_LEN(STR))//' Calibration for delay rate is not a number. '// &
     &              'Probably, database or scratch file are corrupted', INT2(0), &
     &               INT2(0) )
                STOP 'CRES  Abnormal termination'
             ELSE IF ( NORATE_FLAG  .AND.  IS_R8_NAN( RT ) ) THEN
!
! ------------- Corrective action
!
                RT = 0.0D0
           END IF
         ELSE
!
! -------- This section is added in order to prevent work with unitilaized
! -------- variable. In the case of the first observation of the session
! -------- was non-detected, get_calib did not run and result was
! -------- unpredicatble. It may trigger float point exception
!
           DERR_RAW  = -1.0D0
           RERR_RAW  = -1.0D0
           DPHER_RAW = -1.0D0
      END IF
!
! ---- Collecting these changes
!
       COR_TAU  = DT/1.D6
       COR_RATE = RT
!
! ---- Calculation of additive weight corrections. They should be positive
! ---- or zero in the case of normal work. However it is possible situation
! ---- when this correction appeared to be imaginary. In the case when the
! ---- effective frequency is less than 5GHz, GET_CALIB consider input
! ---- DERR as a formal error of group delay at the S-band. It recalculte
! ---- DERR on the fly and output DERR is formal error of ionosphere
! ---- free linear comination of X- and S- band observables + additive
! ---- correction. DERR_RAW, DPHER_RAW, RERR_RAW kept up to now values
! ---- of formal error of ionosphere free linear combination without
! ---- applying baseline-dependent and source dependent quadratical
! ---- correction to weithts
!
       IF ( DERR - DERR_SAVE .GT. -1.D-14 ) THEN
            ADDERR_GR_TAU = DSQRT ( DABS ( DERR**2  - DERR_SAVE**2  ) )
            DERR_RAW      = DSQRT ( DERR_SAVE**2    + DERR_RAW**2  )
          ELSE
!
! --------- Special trick to handle S-band situation. Postfix X should not
! --------- embarass -- it means here S-band (It is true, I don't lie!!)
!
            ADDERR_GR_TAU = DSQRT ( DABS ( DERR**2  - DERR_RAW**2  ) )
            TAUGR_ERR_X   = DERR_RAW
       END IF
!
       IF ( DPHER - DPHER_SAVE .GT. -1.D-14 ) THEN
            ADDERR_PH_TAU = DSQRT ( DABS ( DPHER**2 - DPHER_SAVE**2 ) )
            DPHER_RAW     = DSQRT ( DPHER_SAVE**2 + DPHER_RAW**2 )
         ELSE
            ADDERR_PH_TAU = DSQRT ( DABS ( DPHER**2 - DPHER_RAW**2 ) )
            TAUPH_ERR_X   = DPHER_RAW
       END IF
!
       IF ( RERR - RERR_SAVE .GT. -1.D-16 ) THEN
            ADDERR_RATE = DSQRT ( DABS ( RERR**2  - RERR_SAVE**2  ) )
            RERR_RAW    = DSQRT ( RERR_SAVE**2  + RERR_RAW**2  )
          ELSE
            ADDERR_RATE = DSQRT ( DABS ( RERR**2  - RERR_RAW**2   ) )
            RATE_ERR_X  = RERR_RAW
       END IF
!
       IF ( IONO_ERR_FCT > 0.0D0 .OR. ( FL_VTD_IONO .AND. L_TCN > 0 ) ) THEN
            STA_NAM(1) = ISITN_CHR(ISITE(1))
            STA_NAM(2) = ISITN_CHR(ISITE(2))
            CALL VTD_NAME_REPAIR ( STA_NAM(1) )
            CALL VTD_NAME_REPAIR ( STA_NAM(2) )
!
            CALL ERR_PASS ( IUER, IER )
            CALL GET_IONO_AVR_COV ( %VAL(VTD_ADR), &
     &               ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &               IONO_ZEN_AVR, IONO_ZEN_COV, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8943, IUER, 'SECND', 'Error in an '// &
     &               'attempt to extract ionosphere path delay '// &
     &               'statistics' )
                 RETURN 
            END IF
            DEL_GPS_IONO(1) = DER_DEL(VTD__IONO1)
            DEL_GPS_IONO(2) = DER_DEL(VTD__IONO2)
            CALL APPLY_TCN ( L_TCN, TCN, STA_NAM, IONO_ZEN_AVR, &
     &                       DEL_GPS_IONO, FREQ_GR_X, ELEV, DER_DEL, &
     &                       ADDERR_GR_TAU )
          ELSE
            IONO_ZEN_COV = 0.0D0
       END IF
       IF ( FL_VTD_IONO .AND. IONO_ERR_FCT > 0.0D0 ) THEN
!            IONOV_GE(1) = IONO_ERR_FCT * &
!     &                      DSQRT ( MAX (         IONO_ZEN_COV(1)*DER_DEL(VTD__DER_IONO1)**2 &
!     &                                    - 2.0D0*IONO_ZEN_COV(2)*DER_DEL(VTD__DER_IONO1)*DER_DEL(VTD__DER_IONO2) &
!     &                                    +       IONO_ZEN_COV(3)*DER_DEL(VTD__DER_IONO2)**2, &
!     &                              0.0D0 ) )
!            IONOV_GE(2) = IONOV_GE(1)
            CONTINUE
          ELSE IF ( FL_VTD_IONO .AND. IONO_ERR_FCT < -1.0D0 ) THEN
            IONO_VLBI = -(TAUGR_OBS_X - TAUGR_OBS_S)*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
            COR_TAU = COR_TAU + (IONO_GPS - IONO_VLBI)/IONO_ERR_FCT
       END IF
       TAUGR_ERR_X = SESS_REWEI_SCALE*TAUGR_ERR_X
       TAUGR_ERR_S = SESS_REWEI_SCALE*TAUGR_ERR_S
!
       IF ( FL_ADDW ) THEN
            ADDW_SES_SCL = ADDW_SCL
            IF ( FL_ADDW_IONO ) THEN
!
! -------------- Case when we have additive weigth corrections due to ionospheric 
! -------------- contribution. These additive weigtht corrections are compuited 
! -------------- at frequency ADDW_FRQ which in general not the same as 
! -------------- the effecive ionospheric frequency of the obsevation.
! -------------- Therefore, we scale that additive weight correction by 
! -------------- the ratio of frequencies
!
                 IF ( DATYP_INQ ( IDATYP, XBAND__DTP ) ) THEN
                      ADDW_SES_SCL = ADDW_SCL * (ADDW_FRQ/(1.D6*EFFREQ))**2
                    ELSE IF ( DATYP_INQ ( IDATYP, SBAND__DTP ) ) THEN
                      ADDW_SES_SCL = ADDW_SCL * (ADDW_FRQ/(1.D6*EFFREQ_XS))**2
                 END IF
            END IF
!
! --------- Apply external additive in quadrature reciprocal weight correction
!
!@            IF ( FL_ADDW_BW ) THEN
!@!
!@! -------------- Baseline weights are considered included in the external weights.
!@! -------------- The total baseline weight is the maximum among the baseline weight and
!@! -------------- additive weight.
!@!
!@                 ADDERR_GR_TAU = MAX ( ADDW_SES_SCL*ADDW_EXT(NOBS), ADDERR_GR_TAU )
!@               ELSE
!@!
!@! --------------- External weights and baseline weights are considered independent
!@!
!@                 ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + (ADDW_SES_SCL*ADDW_EXT(NOBS))**2 )
!@            END IF
            IF ( ADDW_USE == ADDW__MAX ) THEN
                  ADDERR_GR_TAU = MAX ( ADDW_SES_SCL*ADDW_EXT(NOBS), ADDERR_GR_TAU )
                ELSE IF ( ADDW_USE == ADDW__MIN ) THEN
                  ADDERR_GR_TAU = MIN ( ADDW_SES_SCL*ADDW_EXT(NOBS), ADDERR_GR_TAU )
                ELSE IF ( ADDW_USE == ADDW__QUAD ) THEN
                  ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + (ADDW_SES_SCL*ADDW_EXT(NOBS))**2 )
                ELSE IF ( ADDW_USE == ADDW__REPL ) THEN
!
! ----------------- This case is considered later
!
                  CONTINUE                
            END IF
       END IF
       ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + SESS_REWEI_QUADR**2 )
!
       IF ( FUSED_STATUS == IONOV__LOADED .AND. &
     &      .NOT. FL_SUP(NOBS)              .AND. &
     &      ( DATYP_INQ ( IDATYP, FUSED__DTP  ) .OR. ILAST_OBORG_I2 == 7777 ) ) THEN
!
! --------- Compute delay corrections and additive in quadrature uncertainty
! --------- in the fused data type
!
            IF ( BTEST ( DTEC_FLG, DTHL__STS ) ) THEN
!
! -------------- Both band provided data usable when the ionospheric mode was computed
!
                 CONTINUE 
               ELSE IF ( BTEST ( DTEC_FLG, DTH__STS ) ) THEN
!
! -------------- Only the upper band provided data usable when the ionospheric mode was computed
!
                 COR_TAU = COR_TAU + (TEC_APR(2) - TEC_APR(1) + DTEC_ADJ)* &
     &                               VIO__CONST/FREQ_GR_X**2 - &
     &                               DEL_BIAS_UL*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
                 ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + (DTEC_ERR*VIO__CONST/FREQ_GR_X**2)**2 )
               ELSE IF ( BTEST ( DTEC_FLG, DTL__STS ) ) THEN
!
! -------------- Only the lower band provided data usable when the ionospheric mode was computed
!
                 COR_TAU = COR_TAU + (TEC_APR(2) - TEC_APR(1) + DTEC_ADJ)* &
     &                               VIO__CONST/FREQ_GR_S**2 - &
     &                               DEL_BIAS_UL*FREQ_GR_X**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
                 ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + (DTEC_ERR*VIO__CONST/FREQ_GR_S**2)**2 )
            END IF
          ELSE IF ( FUSED_STATUS == IONOV__LOADED  .AND. &
     &             .NOT. FL_SUP(NOBS)              .AND. &
     &             ILAST_OBORG_I2 == 8888                ) THEN
            IONO_VLBI = -(TAUGR_OBS_X - TAUGR_OBS_S)*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
            COR_TAU  = COR_TAU - (TEC_APR(2) - TEC_APR(1))* VIO__CONST/FREQ_K**2   &
     &                         + IONO_VLBI*FREQ_GR_X**2/FREQ_K**2 &
     &                         + DEL_BIAS_UL*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)*FREQ_GR_X**2/FREQ_K**2 
          ELSE IF ( FUSED_STATUS == IONOV__LOADED  .AND. &
     &             .NOT. FL_SUP(NOBS)              .AND. &
     &             ILAST_OBORG_I2 == 9999                ) THEN
            IONO_VLBI = -(TAUGR_OBS_X - TAUGR_OBS_S)*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
            COR_TAU  = COR_TAU - 0.784D0*(TEC_APR(2) - TEC_APR(1))* VIO__CONST/FREQ_K**2   &
     &                         + IONO_VLBI*FREQ_GR_X**2/FREQ_K**2 &
     &                         + DEL_BIAS_UL*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)*FREQ_GR_X**2/FREQ_K**2 
       END IF
!
       IF ( DATYP_INQ ( IDATYP, GX__DTP  )               .AND. &
     &      ( TEC_SCAL .NE. 0.0 .OR. TEC_BIAS .NE. 0.0 )       ) THEN
!
! --------- Apply the contribution  of TEC scale and bias to path delay for X-band group delay observable
!
            COR_TAU = COR_TAU + ( TEC_SCAL*( TEC_APR(2)              - TEC_APR(1)              ) + &
     &                            TEC_BIAS*( DER_DEL(VTD__DER_IONO2) - DER_DEL(VTD__DER_IONO1) )   &
     &                          )* VIO__CONST/FREQ_GR_X**2 
       END IF
       IF ( DATYP_INQ ( IDATYP, GS__DTP  )               .AND. &
     &      ( TEC_SCAL .NE. 0.0 .OR. TEC_BIAS .NE. 0.0 )       ) THEN
!
! --------- Apply the contribution  of TEC scale and bias to path delay for S-band group delay observable
!
            COR_TAU = COR_TAU + ( TEC_SCAL*( TEC_APR(2)              - TEC_APR(1)              ) + &
     &                            TEC_BIAS*( DER_DEL(VTD__DER_IONO2) - DER_DEL(VTD__DER_IONO1) )   &
     &                          )* VIO__CONST/FREQ_GR_S**2 
       END IF
       IF ( DTEC_SBA_USE ) THEN
!
! --------- A special mode when the contribution of the TEC adjustment is applied
!
            IF ( DATYP_INQ ( IDATYP, GX__DTP  ) ) THEN
!
! -------------- Upper band
!
                 COR_TAU = COR_TAU + DTEC_ADJ* VIO__CONST/FREQ_GR_X**2
                 ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + (DTEC_ERR_SCL*DTEC_ERR*VIO__CONST/FREQ_GR_X**2)**2 )
            END IF
            IF ( DATYP_INQ ( IDATYP, GS__DTP  ) ) THEN
!
! -------------- Low band
!
                 COR_TAU = COR_TAU + DTEC_ADJ* VIO__CONST/FREQ_GR_S**2
                 ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + (DTEC_ERR_SCL*DTEC_ERR*VIO__CONST/FREQ_GR_S**2)**2 )
            END IF
       END IF
!
       IF ( ILAST_OBORG_I2 == 6666  ) THEN
            COR_TAU  = COR_TAU + DTEC_ADJ
!!            ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + DTEC_ERR**2 )
       END IF
       IF ( ILAST_OBORG_I2 == 7777 .AND. BTEST ( DTEC_FLG, DTH__STS ) ) THEN
            IDATYP_SAVE = IDATYP
            IDATYP = GX__DTP 
       END IF
       IF ( ILAST_OBORG_I2 == 7777 .AND. BTEST ( DTEC_FLG, DTL__STS ) ) THEN
            IDATYP_SAVE = IDATYP
            IDATYP = GS__DTP 
       END IF
!
! ---- Making O-C
!
       CALL MAKE_OC ( TAU_CALC, RATE_CALC, COR_TAU, &
     &                COR_RATE, ADDERR_GR_TAU, ADDERR_PH_TAU, ADDERR_RATE, &
     &                TAUGR_OBS_X, TAUGR_OBS_S, TAUPH_OBS_X, TAUPH_OBS_S, &
     &                TAUSB_OBS_X, TAUSB_OBS_S, TAUGR_ERR_X, TAUGR_ERR_S, &
     &                TAUPH_ERR_X, TAUPH_ERR_S, TAUSB_ERR_X, TAUSB_ERR_S, &
     &                RATE_OBS_X,  RATE_OBS_S,  RATE_ERR_X,  RATE_ERR_S, &
     &                FREQ_GR_X,   FREQ_GR_S,   FREQ_PH_X,   FREQ_PH_S, &
     &                FREQ_RATE_X, FREQ_RATE_S, &
     &                AUTO_SUP, USER_SUP, USER_REC, DTEC_FLG, &
     &                IDATYP, OPP_STATUS, PAMB_STATUS, &
     &                TAU_OC, RATE_OC, TAU_E, RATE_E )
       IF ( ILAST_OBORG_I2 == 7777 .AND. BTEST ( DTEC_FLG, DTH__STS ) ) IDATYP = IDATYP_SAVE 
       IF ( ILAST_OBORG_I2 == 7777 .AND. BTEST ( DTEC_FLG, DTL__STS ) ) IDATYP = IDATYP_SAVE 
!
! ---- Restoring modified variables. They appeared to be calibrated.
!
       DT = DT_SAVE + COR_TAU*1.D6
       RT = RT_SAVE + COR_RATE
!
       IF ( FL_AOC ) THEN
!
! --------- Apply a priori observation correction
!
            TAU_OC = TAU_OC + AOC_DEL(NOBS)
       END IF
!
       IF ( INDEX ( SOLVE_DEBUG, 'IONO_TEST' ) > 0  .AND. &
     &      FL_VTD_IONO                             .AND. &
     &      L_TCN > 0                                     ) THEN
            CALL IONO_TEST_NOI ( L_TCN, TCN, STA_NAM, IONO_ZEN_AVR, &
     &                           DEL_GPS_IONO, FREQ_GR_X, ELEV, &
     &                           RAN_SEED_I4, 100.0D0, TAU_OC )
       END IF
       IF ( SOLVE_DEBUG(1:2) == '66' ) THEN
            IF ( DER_DEL(VTD__COR_FD1) > 0.01D0 ) THEN
                 TAU_E = DSQRT ( (TAU_E**2 - TAUGR_ERR_X**2) + &
     &                           (TAUGR_ERR_X/DER_DEL(VTD__COR_FD1))**2 )
               ELSE IF ( DER_DEL(VTD__STRUC) .NE. 0.0D0 ) THEN
                  TAU_E = 1.D-8
                  TAU_OC = TAU_OC + DER_DEL(VTD__STRUC)
            END IF
      END IF
!
! --- Calculation the current equation of conditions
!
      CALL F__CLR_IND ( 0, F__NONE, PLACE, B3DOBJ, B1B3DOBJ )
      CALL PARTL ( DERIV, POLYONLY, PLACE, B3DOBJ )
!
      IF ( FL_EXT_ERR ) THEN
           TAU_E = EXT_ERR(NOBS)
           IF ( TAU_E > TAU_E_MAX ) THEN
                USER_SUP = IBSET ( USER_SUP, INT4(IDATYP) )
           END IF
      END IF
      IF ( ELEV(1) < ELEV_DWNT .OR. ELEV(2) < ELEV_DWNT ) THEN
!
! -------- Elevation angle downweighting
!
           TAU_E = TAU_E * DWNT_FACTOR
      END IF 
      IF ( (FJD+FRACTC - TMIN)*86400.0D0 - EPS_SEC < TIM_UPWEI_BEG .OR. &
     &     (FJD+FRACTC - TMIN)*86400.0D0 + EPS_SEC > TIM_UPWEI_END      ) THEN
           TAU_E = TAU_E * DWNT_FACTOR
      END IF
      IF ( FL_ADDW ) THEN
           IF ( ADDW_USE == ADDW__REPL ) THEN
                TAU_E = ADDW_EXT(NOBS)
           END IF
      END IF
!
      IF ( SUPMET == SUPMET__META ) THEN
           CALL AUTO_SUP_UPD ( ISITE, ISTAR, ELEV, AUTO_SUP )
         ELSE
!
! -------- Setting flags of suppression status
!
           CALL SUPSTAT_SET ( IUNW, IUNWP, LQUAL, LQUALXS, &
     &                        ICORR, GIONSG, PHIONS, IWVBIT1, ISITE, &
     &                        JSITI, ITT, ISTAR, ELEV, KIONO, SNR, SNR_S, &
     &                        SUPSTAT, UACSUP )
!
! -------- Setting IUNW and IUNWP in according with values of UACSUP.
! -------- We do it in trying to provide backward compatibility with
! -------- previous (before MAY98) version of SOLVE
!
           CALL SUPSTAT_UNW ( SUPSTAT, UACSUP, IUNW, IUNWP )
      END IF
!
! --- If elevation cutoff is negative, then downweight low elevation
! --- observations
!
      IF ( ELEV(1) < -ELVCUT(ISITE(1)) ) THEN
           TAU_E = DSQRT ( TAU_E**2 + 2.D-8**2 )
      END IF
      IF ( ELEV(2) < -ELVCUT(ISITE(2)) ) THEN
           TAU_E = DSQRT ( TAU_E**2 + 2.D-8**2 )
      END IF
!
      IF ( EDC_USE == EDC__REQ .OR. &
     &     EDC_USE == EDC__USE      ) THEN
!
! -------- Post OCT2007 decimation. Set decimation bits in suppression
! -------- bit field
!
           IF ( EDC_USE == EDC__REQ ) CALL ERR_PASS ( IUER, IER )
           IF ( EDC_USE == EDC__USE ) CALL ERR_PASS ( 0,    IER )
           CALL EDC_SET  ( EDC, EDC_PAR, NOBS, ISITE, ISTAR, &
     &                     MJD_TAI_OBS, TAI_OBS, SUPSTAT, AUTO_SUP, IER )
           IF ( IER .NE. 0  .AND. EDC_USE == EDC__REQ ) THEN
                CALL ERR_LOG ( 8944, IUER, 'SECND', 'Error in EDC_SET' )
                RETURN
           END IF
      END IF
      IF ( ( DATYP_INQ ( IDATYP, XBAND__DTP ) .OR. &
     &       DATYP_INQ ( IDATYP, COMB__DTP  ) .OR. &
     &           FL_USED_AS_XS                .OR. &
     &           FL_USED_AS_X                      ) .AND. &
     &     BAD_OBS ( LQUAL_CHR )                           ) THEN
!
           TAU_OC = 0.0D0
           TAU_E  = 0.0D0
           TAUGR_OBS_X = 0.0D0
           RATE_OBS_X  = 0.0D0
           TAUGR_ERR_X = 0.0D0
           USER_SUP = IBSET ( USER_SUP, INT4(IDATYP) )
      END IF
!
      IF ( (     DATYP_INQ ( IDATYP, SBAND__DTP  ) .OR. &
     &           DATYP_INQ ( IDATYP, COMB__DTP   ) .OR. &
     &           FL_USED_AS_XS                          ) .AND. &
     &     .NOT. DATYP_INQ ( IDATYP, IOCAL__DTP  )        .AND. &
     &     BAD_OBS ( LQUALXS_CHR )                              ) THEN
!
           TAU_OC = 0.0D0
           TAU_E  = 0.0D0
           TAUGR_OBS_S = 0.0D0
           RATE_OBS_S  = 0.0D0
           TAUGR_ERR_S = 0.0D0
           USER_SUP = IBSET ( USER_SUP, INT4(IDATYP) )
      END IF
!
      IF ( SUPMET == SUPMET__META ) THEN
           FL_USED = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
           FL_GOOD = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, GOOD__SPS )
           FL_RECO = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, RECO__SPS )
       ELSE
           FL_USED = SUPR_INQ ( SUPSTAT, UACSUP, USED__SPS )
           FL_GOOD = SUPR_INQ ( SUPSTAT, UACSUP, GOOD__SPS )
           FL_RECO = SUPR_INQ ( SUPSTAT, UACSUP, RECO__SPS )
      END IF
      IF ( FL_USED_AS_XS ) THEN
           IDATYP_SAVE = IDATYP
           IDATYP      = G_GXS__DTP
           FL_USED = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
           IF ( TAU_E  == 0.0D0 ) FL_USED = .FALSE.
           IDATYP = IDATYP_SAVE
      END IF
!
! --- Checking a kludge variable SOLVE_SNR_MIN. If it set, then
! --- observations with low SNR are deselected
!
      FL_SNR = .TRUE.
      IF ( ( DATYP_INQ ( IDATYP, XBAND__DTP ) .OR. &
     &       DATYP_INQ ( IDATYP, COMB__DTP  )      ) .AND. &
     &     SNR < SOLVE_SNR_MIN                             ) THEN
           FL_USED = .FALSE.
           FL_SNR  = .FALSE.
      END IF
      IF ( ( DATYP_INQ ( IDATYP, SBAND__DTP ) .OR. &
     &       DATYP_INQ ( IDATYP, COMB__DTP  )      ) .AND. &
     &     SNR_S < SOLVE_SNR_MIN                           ) THEN
           FL_USED = .FALSE.
           FL_SNR  = .FALSE.
      END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!           IONO_VLBI = -(TAUGR_OBS_X - TAUGR_OBS_S)*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
!           WRITE  ( 6, 249 ) nobs, 1.D12* ( AOC_DEL(NOBS) + IONO_VLBI ), DBNAME_CH, FL_USED
! 249       format ( 'KKK- nobs= ', i5, ' dd= ', f12.2, ' dbname: ', A, ' fl_used: ', L1 )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      IF ( FL_USED ) THEN
!
! -------- Update accumulators for weighted mean epochs over all observations
!
           WW_ALL = WW_ALL + 1.D0/TAU_E
           IF ( JDATE_ALL_BEG .LT. -BIG_VALUE/2.0 ) JDATE_ALL_BEG = FJD + FRACTC
           JDATE_ALL_END = (FJD + FRACTC)
!
! -------- We subtract JDATE_ALL_BEG in order to avoid of loss of precision
! -------- with operating with large numbers
!
           JDATE_ALL_MID = JDATE_ALL_MID + (FJD + FRACTC - JDATE_ALL_BEG )/TAU_E
!
! -------- ... and over observations of participating stations
!
           DO 4100 J10=1,2
              WW_STA(ISITE(J10)) = WW_STA(ISITE(J10)) + 1.D0/TAU_E
              IF ( JDATE_STA_BEG(ISITE(J10)) .LT. -BIG_VALUE/2.0 ) THEN
                   JDATE_STA_BEG(ISITE(J10)) = (FJD + FRACTC)
              END IF
              JDATE_STA_MID(ISITE(J10)) = JDATE_STA_MID(ISITE(J10)) + &
     &                                   (FJD + FRACTC - JDATE_ALL_BEG )/TAU_E
              JDATE_STA_END(ISITE(J10)) = (FJD + FRACTC)
 4100      CONTINUE
      END IF
!
! --- Setting the value of the variable IRUNW
!
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
           IRUNW_SET = IUNWP
        ELSE
           IRUNW_SET = IUNW
      END IF
      IRUNW=0
      IF ( IRUNW_SET.EQ.1) THEN
           CALL SBIT ( IRUNW, INT2(1), INT2(1) )
        ELSE IF ( IRUNW_SET.EQ.2 ) THEN
           CALL SBIT ( IRUNW, INT2(2), INT2(1) )
        ELSE IF ( IRUNW_SET.EQ.3 ) THEN
           CALL SBIT ( IRUNW, INT2(4), INT2(1) )
        ELSE IF ( IRUNW_SET.EQ.4 ) THEN
           CALL SBIT ( IRUNW, INT2(3), INT2(1) )
        ELSE IF ( IRUNW_SET.EQ.8 ) THEN
           CALL SBIT ( IRUNW, INT2(5), INT2(1) )
        ELSE IF ( IRUNW_SET.EQ.12 ) THEN
           CALL SBIT ( IRUNW, INT2(6), INT2(1) )
        ELSE IF ( IRUNW_SET.EQ.16 ) THEN
           CALL SBIT ( IRUNW, INT2(7), INT2(1) )
        ELSE IF ( IRUNW_SET.EQ.17 ) THEN
           CALL SBIT ( IRUNW, INT2(8), INT2(1) )
      ENDIF
!
! --- If SOCAL returns with NOGOOD=1 (i.e., ionospheric correction not
! --- good) then give the observation an unweight value of 8 so that
! --- the statistics calculations will reflect only good data.  Note,
! --- however, that this is only a temporary downweighting since CRES
! --- does not write the OBSFIL back out.  This also causes the RESFIL
! --- unweight flag to be set to 8 so that the ionosphere downweight
! --- information will be passed to CNPLT.
!
      IF ( CNPLT_SUPR_PRE98 ) THEN
           IF ( DATYP_INQ ( IDATYP, GROUP__DTP )  .OR. &
     &          DATYP_INQ ( IDATYP, SINGL__DTP )        ) THEN
!
! ------------- Group or narrow-band delay data
!
                IF ( NOGOOD.EQ.1 ) THEN
                     CALL SBIT ( IRUNW, INT2(5), INT2(1) )
                     IF ( IUNW.EQ.0 ) IUNW = 8
                ENDIF
              ELSE IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! ------------- Phase delay data
!
                IF ( NOGOOD.EQ.1 ) THEN
                     CALL SBIT ( IRUNW, INT2(5), INT2(1) )
                     IF ( IUNWP.EQ.0 ) IUNWP = 8
                ENDIF
              ELSE IF ( DATYP_INQ ( IDATYP, RATONL__DTP ) ) THEN
!
! ------------- Rate only
!
                IF ( NOGOOD.EQ.1 ) THEN
                     CALL SBIT ( IRUNW, INT2(5), INT2(1) )
                     IF ( IUNW.EQ.0 ) IUNW = 8
                ENDIF
           END IF
      END IF
!
! --- Store the phase unweight flag in IUNW if processing phase delays
! --- so that the statistics are correctly handled for this case and so
! --- that the unweight flag stored in RESFIL represents the correct
! --- data type.  Note that this operation is transient since CRES does
! --- not rewrite the OBSFIL.
!
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) IUNW = IUNWP
!
! --- Perform the elevation cutoff check and WVR mask check
!
      IF ( ( ELEV(1) .LT. ELVCUT(ISITE(1)) ) .OR. &
     &     ( ELEV(2) .LT. ELVCUT(ISITE(2)) )      ) THEN
             CALL SBIT ( IRUNW, INT2(4), INT2(1) )
             IF ( IUNW.EQ.0 ) IUNW = 3
      ENDIF
!
!@      IF( (   WVMASK(ISITE(1)) .NE. 0 ) .AND. &
!@     &    ( ( IWVBIT1(1) .AND. WVMASK(ISITE(1)) ) .EQ.0 ) ) THEN
!@            CALL SBIT( IRUNW, INT2(6), INT2(1) )
!@            IF (IUNW.EQ.0) IUNW = 12
!@      ENDIF
!@      IF( (WVMASK(ISITE(2)).NE.0) .AND. &
!@     &    ( (IWVBIT1(2).AND.WVMASK(ISITE(2))).eq.0) ) then
!@            CALL SBIT( IRUNW, INT2(6), INT2(1) )
!@            IF (IUNW.EQ.0) IUNW = 12
!@    ENDIF
!
      IF ( IUNW .EQ. 0 ) THEN
!
! -------- Pre1996 archaic decimation?
!
           OBCOUNTR = OBCOUNTR + 1
           IF ( EVINT4 .GT. 1  .AND.  EVINT4 .LE. MAX_OBS ) THEN
                IF ( EVSTART4 .NE. MOD(OBCOUNTR,EVINT4) ) THEN
                     IUNW = 8
                ENDIF
           ENDIF
      ENDIF
!
! === Calculate the delay residual
!
      DOC   = TAU_OC
      DOERR = TAU_E
      DOERR_RAW = SQRT ( DERR_RAW**2 + REWEIGHT_FLOOR**2 )
!
      IF ( DATYP_INQ ( IDATYP, PHSRAT__DTP ) ) THEN
!
! -------- Special case when phase delay data are produced from group delays
! -------- data on the fly
!
           IF ( IRESTYP .EQ. 1 ) THEN
                DOC = DPH - DPHXS
                IF ( DPHXS .LT. 1.0D-24 ) THEN
                     IUNWP = 1
                     CALL SBIT ( IRUNW, INT2(1), INT2(1) )
                ENDIF
                DOERR = SQRT ( DPHER**2 + DPHERXS**2 )
                IF (KBIT( JSITI(ITT(ISITE(1)) ), INT2(4) ) .AND. &
     &          KBIT( JSITI(ITT(ISITE(2)) ), INT2(4) )       ) THEN
!
                    DOC = DOC + (DOBS - DOBSXS)
                    DOERR = SQRT ( DPHER_RAW**2 + DPHERXS**2 + &
     &                             DERR_RAW**2  + DOBSXS**2    )
                    DOERR_RAW = DOERR
                    IF ( KBIT( ICORR, INT2(5)) .OR. KBIT( ICORR, INT2(6) )) &
     &                   THEN
                         IUNWP = 8
                         CALL SBIT ( IRUNW, INT2(5), INT2(1) )
                    ENDIF
                END IF
!
                IF ( KBIT( JSITI(ITT(ISITE(1))), INT2(5)) .AND. &
     &               KBIT( JSITI(ITT(ISITE(2))), INT2(5))       ) THEN
!
                     DOC = DOC - (DPH - DPHXS)
                     DOERR = SQRT ( DPHER_RAW**2 + DPHERXS**2 + &
     &                              DPHER_RAW**2 + DPHERXS**2   )
                     DOERR_RAW = DOERR
                     IF ( KBIT( ICORR, INT2(11)) .OR. KBIT( ICORR, INT2(12)) &
     &                    .AND.IUNW.EQ.0)                                    &
     &                    THEN
!
                          IUNWP=8
                          CALL SBIT ( IRUNW, INT2(5), INT2(1) )
                     ENDIF
                END IF
           END IF
!
! -------- Phase delay - group delay
!
           IF ( IRESTYP .EQ. 2 ) THEN
                DOC = DPH - DOBS
                DOERR = SQRT ( DPHER**2 + DERR**2 )
                IF ( IUNW.NE.0 .AND. IUNWP.EQ.0 ) IUNWP = IUNW
!
! ------------- Group iono
! ------------- Note:  dpher_raw**2 already has one factor of
! ------------- gion(1)**2 from socal!
!
               IF ( KBIT( JSITI(ITT(ISITE(1))), INT2(4) ) .AND. &
     &              KBIT( JSITI(ITT(ISITE(2))), INT2(4) )       ) THEN
                    TEMP=1.D0 + EFFREQ**2/PHEFFREQ**2
                    DOC = DOC + GION(1)*TEMP
                    IF ( EFFREQ .GT. 5000. ) THEN
                         DOERR = SQRT ( DPHER_RAW**2 + &
     &                                  DERR_RAW**2* ( 1. + 2.*.078*TEMP ) + &
     &                                  GIONSG(1)**2*( TEMP**2 -1))
                       ELSE
                         DOERR = SQRT ( DPHER_RAW**2 + &
     &                                  DERR_RAW**2* ( 1. - 2.*1.078*TEMP)+ &
     &                                  GIONSG(1)**2*( TEMP**2 -1) )
                    ENDIF
                    DOERR_RAW = DOERR
                    IF ( KBIT( ICORR, INT2(5)) .OR. KBIT( ICORR, INT2(6) )) &
     &                   THEN
                         IUNWP=8
                         CALL SBIT ( IRUNW, INT2(5), INT2(1) )
                    ENDIF
               END IF
!
! ------------ Phase iono cal
!
               IF ( KBIT( JSITI(ITT(ISITE(1))), INT2(5)) .AND. &
     &              KBIT( JSITI(ITT(ISITE(2))), INT2(5))        ) THEN
                    DOC = DOC + PHION + PHION*(PHEFFREQ**2/EFFREQ**2)
                    DOERR = SQRT ( DPHER_RAW**2 + PHIONS**2 + &
     &                             DERR_RAW**2  + PHIONS**2   )
                    DOERR_RAW = DOERR
                    IF ( KBIT( ICORR, INT2(11)) .OR. KBIT( ICORR, INT2(12)) &
     &                   .AND.IUNW.EQ.0) THEN
                         IUNWP=8
                         CALL SBIT ( IRUNW, INT2(5), INT2(1) )
                    ENDIF
               END IF
           END IF
!
! -------- Scaling from microsec to sec
!
           DOC = DOC / 1.0D6
      END IF
!
      IF (   DATYP_INQ ( IDATYP,  PHASE__DTP )      .AND. &
     &     ( IRESTYP .EQ. 1  .OR.  IRESTYP .EQ. 2 )       ) THEN
!
! -------- Don't apply theoretical adjustments for special phase residual types
! -------- when phase delay observables are produced on the fly
!
         ELSE
!
! -------- Apply theoretical adjustments for any other cases
!
           IF ( FAST_MODE .EQ. F__NONE  ) THEN
                DD = DDOT ( NBLAS, DERIV(1,1), IBLAS1, ARR(JB), IBLAS1 )
              ELSE IF ( FAST_MODE .EQ. F__PRD   .OR. &
     &                  FAST_MODE .EQ. F__B3D   .OR. &
     &                  FAST_MODE .EQ. F__B1D   .OR. &
     &                  FAST_MODE .EQ. F__B1B3D      ) THEN
!
! ------------- (sparse) dot product of the vector of residuals and vector of
! ------------- adjustments
!
                DD = SPARSE_DOT ( NPARAM, DERIV(1,1), ARR(JB), &
     &                            PLACE%N_GEN, PLACE%IND_GEN, VC1, VC2 )
           END IF
!
! -------- Subtract contribution of adjustments from o-c for delay
!
           DOC = DOC - DD
      END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!       IF ( FUSED_STATUS == IONOV__LOADED .AND. &
!     &      .NOT. FL_SUP(NOBS)              .AND. &
!     &      ( DATYP_INQ ( IDATYP, FUSED__DTP  ) .OR. ILAST_OBORG_I2 == 7777 ) ) THEN
!!
!            IF ( BTEST ( DTEC_FLG, DTL__STS ) ) THEN
!                 DEL_MOD = (TEC_APR(2) - TEC_APR(1) + DTEC_ADJ)*VIO__CONST/FREQ_GR_S**2
!                 DEL_DAT = (DEL_BIAS_UL - (DOBS/1.D6 - DOBSXS/1.D6) )*FREQ_GR_X**2/( FREQ_GR_X**2 - FREQ_GR_S**2 )
!                 WRITE ( 6, 273 ) DBNAME_CH(1:10), NOBS, ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), ISTRN_CHR(ISTAR), &
!     &                            1.D12*DEL_MOD, 1.D12*DEL_DAT, 1.D12*(DEL_MOD - DEL_DAT), &
!     &                            1.D12*DOC, 1.D12*TAU_E
! 273             FORMAT ( 'CRES-iono ', A10, ' nobs= ', I5, ' sta: ', A, ' / ', A, ' sou: ', A, &
!     &                    ' Del_mod: ', F9.1, ' Del_vlbi: ', F9.1, ' Mod_m_VLBI= ', 0PF9.1, &
!     &                    ' Res: ', 0PF9.1, ' Err: ', 0PF9.1 )
!            END IF
!        END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! --- Process the delay rate data
!
      ROC = ROBS - RT
!
      IF ( .NOT. NORATE_FLAG ) THEN
           IF ( FAST_MODE .EQ. F__NONE ) THEN
                DR =  DDOT ( NBLAS, DERIV(1,2), IBLAS1, ARR(JB), IBLAS1 )
             ELSE IF ( FAST_MODE .EQ. F__PRD   .OR. &
     &                 FAST_MODE .EQ. F__B3D   .OR. &
     &                 FAST_MODE .EQ. F__B1D   .OR. &
     &                 FAST_MODE .EQ. F__B1B3D      ) THEN
!
                DR = SPARSE_DOT ( NPARAM, DERIV(1,2), ARR(JB), PLACE%N_GEN, &
     &                            PLACE%IND_GEN, VC1, VC2 )
           END IF
!
! -------- Substract contribution of adjustments from o-c for rate
!
           ROC = ROC - DR
         ELSE
           ROC = 0.0
      END IF
!
      IF ( SOLVE_DEBUG(1:2) == '48' ) THEN
!
! -------- Printing some intermediate quanitities in the debugging mode
!
           CALL JD_TO_MJD_SEC  ( FJD, MJD_UTC_OBS, UTC_OBS )
           UTC_OBS = UTC_OBS + FRACT*86400.0D0
           IF ( DABS(UTC_M_TAI) < 300.0 ) THEN
                TAI_OBS = UTC_OBS - UTC_M_TAI
              ELSE
                TAI_OBS = UTC_OBS
           END IF
           STR = MJDSEC_TO_DATE ( MJD_UTC_OBS, TAI_OBS, -2 )
           WRITE ( UNIT=LUN_DEBUG, FMT=248 ) NOBS, SCAN_NAME, STR(1:23), &
     &            ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &            ISTRN_CHR(ISTAR), FL_USED, DOC, DOERR, SNR, &
     &            SP(1,1), SP(2,1)
 248       FORMAT ( 'Obs: ', I5, 1X, A, 1X, A, 1X, A8,'/',A8, 1X, A, 1X, &
     &              'Fl_used: ', L1, ' Res: ', F16.13, ' Err: ', 1PD12.5, &
     &              ' SNR: ', 0PF8.1, ' D_ra: ', 1PD14.6, ' D_del: ', 1PD14.6 )
      END IF
!
      IF ( KBIT ( PRE_IP(3), INT2(12))   .OR. &
     &     ( RESOUTFILE(1:4).NE. 'NONE' .AND. KBATCH ) .OR. &
     &     SIGMA_TYPE .NE. 'PR'                              ) THEN
!
! -------- Calculate post_fit correction to residuals. ( JMG 970227  )
! -------- It will work when residual file is generated. This corresction
! -------- will be used for 1) reweighting 2) for buiding normalized residuals
! -------- plot
!
           IF ( DATYP_INQ ( IDATYP, DELAY__DTP ) ) THEN
                POST_FIT_DEL_COR = POST_FIT_COR ( ARR(JA), DERIV(1,1), NPARAM )
             ELSE
                POST_FIT_DEL_COR = 0.
           ENDIF
!
           IF ( DATYP_INQ ( IDATYP, RATE__DTP ) ) THEN
                POST_FIT_RAT_COR = POST_FIT_COR ( ARR(JA), DERIV(1,2), NPARAM )
             ELSE
                POST_FIT_RAT_COR = 0.
           ENDIF
      END IF
!
! --- This computed the post_fit correction exactly
! --- post_fit_del_cor2=post_fit_cor2(ARR(ja),Deriv(1,1),nparam,
! ----    > Cval,ixind,iyind,numc)
!
! --- Extract the times and elevations for printing
!
      IHR  = FRACT * 24.D0 + 0.00000001D0
      IMIN = FRACT * 1440.D0 + 0.00000001D0 - 60.D0 * IHR
      CALL MDYJL ( IM, ID, IY, ITIME, FJD )
      IPELV(1) = ELEV(1)/DEG__TO__RAD + 0.5
      IPELV(2) = ELEV(2)/DEG__TO__RAD + 0.5
      IPAZ(1) = AZ(1)/DEG__TO__RAD + 0.5
      IPAZ(2) = AZ(2)/DEG__TO__RAD + 0.5
      TIME = FJD + FRACT
!
! --- DANGER!!! DOBS has NON-STANDARD units: micorseconds of time
! --- DANGER!!! FOBS now has NON-STANDARD units: nanoseconds of time
!
      FOBS = DOBS * 1.0D3   
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) FOBS = DPH*1.0D3
      IF ( .NOT. KBIT ( IDBSEL,IDB ) ) GOTO 750  ! Database not in solution !
!
! --- Baseline deselection
!
      FL_BAS_DESL = .FALSE.
      FL_SOU_DESL = .FALSE.
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! -------- For phase delay solution type
!
           IF ( .NOT. KBIT ( IBLSEL_P(1,ISITE(1)), ISITE(2) ) ) THEN
                CALL SBIT ( IRUNW, INT2(7), INT2(1) )
                IF ( IUNW.EQ.0 ) IUNW=16
                FL_BAS_DESL = .TRUE.
           ENDIF
        ELSE
!
! -------- For group delay solution type
!
           IF ( .NOT. KBIT ( IBLSEL_G(1,ISITE(1)), ISITE(2) ) ) THEN
                CALL SBIT ( IRUNW, INT2(7), INT2(1) )
                IF ( IUNW.EQ.0 ) IUNW=16
                FL_BAS_DESL = .TRUE.
           ENDIF
      ENDIF
!
! --- Source deselection
!
      IF ( .NOT. KBIT ( ISRSEL, ISTAR) ) THEN
           CALL SBIT ( IRUNW, INT2(8), INT2(1) )
           IF ( IUNW.EQ.0 ) IUNW=17
           FL_SOU_DESL = .TRUE.
      ENDIF
      IF ( CRES_STYLE .EQ. CRES__PRE98 ) THEN
!
! -------- Compatibility mode
!
           IF ( IUNW .NE. 16  .AND.  IUNW .NE. 17 ) THEN
                NUMBER = NUMBER + 1
                CALL STATS ( DOC, DOERR, WRMSI(1), FACT(1), IUNW, SUPMET, &
     &                       FL_USED, NC )
                RERR6 = RERR
                CALL STATS ( ROC, RERR6, WRMSI(2), FACT(2), IUNW, SUPMET, &
     &                       FL_USED, KC )
                IF ( IUNW.EQ.0 ) THEN ! These two site have a used observation
                     GOOD_SITE(ISITE(1)) = .TRUE.
                     GOOD_SITE(ISITE(2)) = .TRUE.
                ENDIF
           END IF
         ELSE
           NUMBER = NUMBER + 1
!
! -------- Set the maximum ceiling for post fit residuals
!
           IF ( DOC .GT.  RES__MAX  ) DOC =  RES__MAX
           IF ( DOC .LT. -RES__MAX  ) DOC = -RES__MAX
!
! -------- Since 29-APR-98 we calculate statistics only for observations which
! -------- actually WERE in solution
!
           IF ( FL_USED ) THEN
                CALL STATS ( DOC, DOERR, WRMSI(1), FACT(1), IUNW, SUPMET, &
     &                       FL_USED, NC )
                WEI = 1.D0/DSQRT( DOERR**2 + ERR_FLOOR**2)
                WEIGHTED_EPOCH(ISTAR) = WEIGHTED_EPOCH(ISTAR) + &
     &                WEI * (((MJD_TAI_OBS - J2000__MJD)*86400.0D0 + TAI_OBS)/(JYEAR__DAYS*86400.0) + 2000.0D0)
                WEIGHT_SUM(ISTAR) = WEIGHT_SUM(ISTAR) + WEI
                RERR6 = RERR
                CALL STATS ( ROC, RERR6, WRMSI(2), FACT(2), IUNW, SUPMET, &
     &                       FL_USED, KC )
                GOOD_SITE(ISITE(1)) = .TRUE.
                GOOD_SITE(ISITE(2)) = .TRUE.
          ENDIF
      ENDIF
      IF ( FL_USED ) IDLT = ' '
!
! --- Form general statistics about
!
! --- ITOTRC_OBS -- potentially recoverable observations
! --- ITOTCG_OBS -- conditionally good observations
! --- ITOTUS_OBS -- used in solution observations
! --- ITOTSU_OBS -- suppressed from solution observations
!
      IF ( FL_RECO ) THEN
           ITOTRC_OBS = ITOTRC_OBS + 1
      END IF
      IF ( FL_GOOD ) THEN
           ITOTCG_OBS = ITOTCG_OBS + 1
      END IF
      IF ( FL_USED ) THEN
           ITOTUS_OBS = ITOTUS_OBS + 1
      END IF
      IF ( SUPMET == SUPMET__META ) THEN
!
! -------- FL_INIT is TRUE if the observation were good without user
! -------- suppression action
!
           FL_INIT = META_SUPR_INQ ( AUTO_SUP, 0, USER_REC, USED__SPS )
           IF ( FL_INIT .AND. .NOT. FL_USED ) THEN
                ITOTSU_OBS = ITOTSU_OBS + 1
           END IF
         ELSE
           IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! ------------- Phase delay case
!
                IF ( SUPR_INQ ( SUPSTAT, UACSUP, GOOD__SPS )  .AND. &
     &                   KBIT ( UACSUP,  PSUP__UAS )                 ) THEN
                     ITOTSU_OBS = ITOTSU_OBS + 1
                END IF
              ELSE
!
! ------------- Group delay case
!
                IF ( SUPR_INQ ( SUPSTAT, UACSUP, GOOD__SPS )  .AND. &
     &                   KBIT ( UACSUP,  GSUP__UAS )                 ) THEN
                     ITOTSU_OBS = ITOTSU_OBS + 1
                END IF
           END IF
      END IF
!
      IF ( SCA_TIM(ISTAR) < -1.D15 ) THEN
!
! -------- Source-dependent scan counter was not initialized
!
           SCA_TIM(ISTAR)  = (MJD_TAI_OBS - MJD_TAI_OBS_BEG)*86400.0D0 + (TAI_OBS - TAI_OBS_BEG)
           SCA_DUR(ISTAR)  = EFF_DUR(1)
           NSCA_TOT(ISTAR) = 1
         ELSE
           IF ( (MJD_TAI_OBS - MJD_TAI_OBS_BEG)*86400.0D0 + (TAI_OBS - TAI_OBS_BEG) > &
     &          SCA_TIM(ISTAR) + SCA_DUR(ISTA) + SCA_STAT_MAX_GAP                     ) THEN
!
! ------------- New observation of this source is later sum of SCA_DUR(ISTA) + SCA_STAT_MAX_GAP
! ------------- and the previous scan start time. Good! Update the counter
!
                NSCA_TOT(ISTAR) = NSCA_TOT(ISTAR) + 1
                SCA_DUR(ISTAR) = EFF_DUR(1)
                SCA_TIM(ISTAR) = (MJD_TAI_OBS - MJD_TAI_OBS_BEG)*86400.0D0 + (TAI_OBS - TAI_OBS_BEG)
           END IF
      END IF
      IF ( FL_USED ) THEN
!
! -------- The same statistics update, but for used observations
!
           IF ( SCA_TIM_USED(ISTAR) < -1.D15 ) THEN
!
! ------------- Source-dependent scan counter was not initialized
!
                SCA_TIM_USED(ISTAR) = (MJD_TAI_OBS - MJD_TAI_OBS_BEG)*86400.0D0 + (TAI_OBS - TAI_OBS_BEG)
                SCA_DUR_USED(ISTAR) = EFF_DUR(1)
                NSCA_USED(ISTAR) = 1
              ELSE
                IF ( (MJD_TAI_OBS - MJD_TAI_OBS_BEG)*86400.0D0 + (TAI_OBS - TAI_OBS_BEG) > &
     &               SCA_TIM_USED(ISTAR) + SCA_DUR_USED(ISTA) + SCA_STAT_MAX_GAP                     ) THEN
!
! ------------------ New observation of this source is later than a sum of SCA_DUR_USED(ISTA) + SCA_STAT_MAX_GAP
! ------------------ and the previous scan start time. Good! Update the counter of used observations
!
                     NSCA_USED(ISTAR) = NSCA_USED(ISTAR) + 1
                     SCA_DUR_USED(ISTAR) = EFF_DUR(1)
                     SCA_TIM_USED(ISTAR) = (MJD_TAI_OBS - MJD_TAI_OBS_BEG)*86400.0D0 + (TAI_OBS - TAI_OBS_BEG)
                END IF
           END IF
      END IF
750   CONTINUE
!
! --- Form up the baseline statistics
!
      IF ( IBCNT .GT. 0 ) THEN
!
! -------- This is not the first baseline
!
           ITEST = 0
           II    = 1
           DO WHILE ( II.LE.IBCNT .AND. ITEST .EQ.0 )
              IF ( (ISITE(1) .EQ. IBAS(1,II) .AND.        &
     &              ISITE(2) .EQ. IBAS(2,II)       ) .OR. &
     &             (ISITE(2) .EQ. IBAS(1,II) .AND. &
     &              ISITE(1) .EQ. IBAS(2,II)       )      ) THEN
                   ITEST = II
                ELSE
                   II = II + 1
              END IF
           END DO
!
! -------- Is this a new baseline?
!
           IF ( ITEST .EQ. 0 ) THEN
                IF ( IBCNT .EQ. MAX_ARC_BSL ) THEN
                     CALL ADDSTR_F(" Too many baselines for CRES" )
                     CALL NL_MN()
                     CALL FERR ( INT2(144), 'Too many baselines', INT2(0), &
     &                    INT2(0) )
                END IF
!
                IBCNT = IBCNT + 1
                IBAS(1,IBCNT) = MIN(ISITE(1),ISITE(2))
                IBAS(2,IBCNT) = MAX(ISITE(2),ISITE(1))
                ITEST = IBCNT
           END IF
        ELSE
!
! -------- Do this part only if this is the first baseline
!
           ITEST = 1
           IBCNT = 1
           II = 1
           IBAS(1,1) = MIN(ISITE(1),isite(2))
           IBAS(2,1) = MAX(ISITE(2),isite(1))
      END IF
!
      IF ( KBIT(IDBSEL,IDB) ) THEN ! Database is in solution
!
! -------- Accumulate the weighted delay residuals, the delay residuals
! -------- weighted with REWEIGHT_FLOOR, and weighted rate residuals.
!
           IF ( ( CRES_STYLE .EQ. CRES__PRE98 .AND. IUNW .NE. 17 )  .OR. &
     &          ( CRES_STYLE .NE. CRES__PRE98 .AND. FL_USED              ) ) THEN
!
! ------------- Note: the loading and unloading of ibas4 is just to accomodate
! -------------       the need for an integer*4 argument to stats.  If ibas is
! -------------       ever made integer*4, it can be passed to stats directly.
!
                IBAS4 = IBAS(3,II)
                CALL STATS ( DOC, DOERR, BW(1,II), BF(1,II), IUNW, SUPMET, &
     &                       FL_USED, IBAS4 )
                IBAS(3,II) = IBAS4
                IBAS4 = IBAS(6,II)
                CALL STATS ( DOC, DOERR_RAW, BW(3,II), BF(3,II), IUNW, SUPMET, &
     &                       FL_USED, IBAS4 )
                IBAS(6,II) = IBAS4
                RERR6 = RERR
                IBAS4 = IBAS(4,II)
                CALL STATS ( ROC, RERR6, BW(2,II), BF(2,II), IUNW, SUPMET, &
     &                       FL_USED, IBAS4 )
                IBAS(4,II) = IBAS4
           ENDIF
           IF ( CRES_STYLE .EQ. CRES__PRE98 ) THEN
!
! ------------- Sum up all points in compatibility mode
!
                IBAS(5,II) = IBAS(5,II) + 1
             ELSE
!
! ------------- Sum except unrecoverable points in POST98 mode
!
                IF ( FL_RECO ) THEN
                     IBAS(5,II) = IBAS(5,II) + 1
                END IF
           END IF
      END IF  ! database is in solutuon
!
! --- Form up the source statistics
!
      IF ( ISCNT .GT. 0 ) THEN
!
! -------- Not the first source encountered
!
           ITEST = 0
           II    = 1
           DO WHILE ( II .LE. ISCNT  .AND.  ITEST .EQ. 0 )
              IF ( ISTAR .EQ. ISRC(1,II) ) THEN
                   ITEST = II
                ELSE
                   II = II + 1
              END IF
           END DO
!
! -------- Is this a new source?
!
           IF ( ITEST .EQ. 0 ) THEN
                IF ( ISCNT .GE. MAX_ARC_SRC ) THEN
                     IPTR = IPTR +1
                     WRITE ( LBUF(IPTR), 9104 ) MAX_ARC_SRC
                     CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                     CALL NL_MN()
 9104                FORMAT (" More than ",I5," sources")
                     CALL FERR ( INT2(145), ' Too many sources for CRES ', &
     &                           INT2(0), INT2(0) )
                END IF
!
                ISCNT = ISCNT + 1
                ISRC(1,ISCNT) = ISTAR
                ITEST = ISCNT
           END IF
        ELSE
!
! -------- Do this part only if this is the first source
!
           ITEST = 1
           ISCNT = 1
           II = 1
           ISRC(1,1) = ISTAR
      END IF
!
! --- Updte statistics counters for this source:
! --- 1st row -- the number of used observations
! --- 2nd row -- the number of recoverable observations
! --- 3rd row -- the total number of observations, including non-detections
!
      IF ( FL_USED ) THEN
           ISTAT_SRC(1,ISTAR) = ISTAT_SRC(1,ISTAR) + 1
      END IF
      IF ( FL_RECO ) THEN
           IF ( FL_SNR ) ISTAT_SRC(2,ISTAR) = ISTAT_SRC(2,ISTAR) + 1
      END IF
      ISTAT_SRC(3,ISTAR) = ISTAT_SRC(3,ISTAR) + 1
!
      IF ( KBIT( IDBSEL, IDB) ) THEN ! Database is in solution
!
! -------- Accumulate the weighted delay residuals, the delay residuals
! -------- weighted with REWEIGHT_FLOOR, and weighted rate residuals.
!
           IF ( ( CRES_STYLE .EQ. CRES__PRE98 .AND. IUNW .NE. 16 .AND. &
     &            IUNW .NE. 17                                         ) .OR. &
     &          ( CRES_STYLE .NE. CRES__PRE98 .AND. FL_USED            ) ) THEN
!
                IBAS4 = ISRC(2,II)
                CALL STATS ( DOC, DOERR, SW(1,II), SF(1,II), IUNW, SUPMET, &
     &                       FL_USED, IBAS4 )
                ISRC(2,II) = IBAS4
                IBAS4 = ISRC(5,II)
                CALL STATS ( DOC, DOERR_RAW, SW(3,II), SF(3,II), IUNW, SUPMET, &
     &                       FL_USED, IBAS4 )
                ISRC(5,II) = IBAS4
                RERR6 = RERR
                IBAS4 = ISRC(3,II)
                CALL STATS ( ROC, RERR6, SW(2,II), SF(2,II), IUNW, SUPMET, &
     &                       FL_USED, IBAS4 )
                ISRC(3,II) = IBAS4
           ENDIF
!
! -------- Sum up all points
!
           ISRC(4,II) = ISRC(4,II) + 1
      END IF
!
! --- Process the residuals for the printer and 'RESFIL'
!
!!!!!!!!!!!!!!!!!!      DOC   = DOC   * 1.0D9
!!!!!!!!!!!!!!!!!!      DOERR = DOERR * 1.0D9
!!!!!!!!!!!!!!!!!!      ROC   = ROC   * 1.0D12
!!!!!!!!!!!!!!!!!!      RERR  = RERR  * 1.0D12
      IF ( FREQ_GR_S .GT. MIN__FRQ*1.0D6 .AND. &
     &     (FREQ_GR_X - FREQ_GR_S) .GT. MIN__FRQ*1.0D6 ) THEN
           COEF_IONO = FREQ_GR_X**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
         ELSE
           COEF_IONO = 1.0D0
      END IF
!
! --- Check all possible unweight flags
!
      IPUNC = IUNC(1)
      IF ( IUNW .EQ. 1   .OR.   IUNW .EQ. 101 ) IPUNC = IUNC(2)
      IF ( IUNW .EQ. 2   .OR.   IUNW .EQ. 102 ) IPUNC = IUNC(4)
      IF ( IUNW .EQ. 100                      ) IPUNC = IUNC(5)
      IF ( IUNW .EQ. 101                      ) IPUNC = IUNC(2)
!
! --- Check for elevation unweight flag
!
      IF ( IUNW .EQ. 3  ) IPUNC = IUNC(3)
!
! --- Check for WVR delete flag
!
      IF ( IUNW .EQ. 12 ) IPUNC = IUNC(14)
!
! --- Check if temporary
!
      IF ( IUNW .EQ. 4  ) IPUNC = IUNC(6)
!
! --- SNR Ratio low
!
      IF ( IUNW .EQ. 5 ) IPUNC = IUNC(7)
!
! --- Ratio of ACC periods between channels
!
      IF ( IUNW .EQ. 6 ) IPUNC = IUNC(8)
!
! --- Total number of ACC periods
!
      IF ( IUNW .EQ. 11 ) IPUNC = IUNC(9)
!
! --- Problem with CAL data
!
      IF ( IUNW .EQ. 7 ) IPUNC = IUNC(10)
!
! --- Problem with residual phases
!
      IF ( IUNW .EQ. 9 ) IPUNC = IUNC(11)
!
! --- SYERR Downweight, point had high weight in solution
!
      IF ( IUNW .EQ. 10 ) IPUNC = IUNC(12)
!
! --- No S-BAND equivalent obs
!
      IF ( IUNW .EQ. 8 ) IPUNC = IUNC( 5)
!
! --- Check weird flags
!
      IF ( IUNW .GT. 11 .AND. IUNW .LT. 100 ) IPUNC = IUNC(13)
      IF ( IUNW .LT. 0                      ) IPUNC = IUNC(13)
!
! --- Preparing and writing record of residuals for either differenced or
! --- normal data
!
      IF ( RESOUTFILE(1:4) .NE. 'NONE'  .AND.  KBATCH ) THEN
           IRESARR(1)  = IARCNM
           IRESARR(2)  = ISITE(1)
           IRESARR(3)  = ISITE(2)
           IRESARR(4)  = ISTAR
           IRESARR(5)  = IY
           IRESARR(6)  = IM
           IRESARR(7)  = ID
           IRESARR(8)  = IHR
           IRESARR(9)  = IMIN
           READ ( UNIT=IPUNC, FMT='(A4)' ) IRESARR(10)
           RRESARR(1)  = 1.D9*DOC
           RRESARR(2)  = 1.D9*DOERR
           RRESARR(3)  = 1.D12*ROC
           RRESARR(4)  = 1.D12*RERR
           JERR = FC_WRITE ( FILDES2, PTR_NC(IRESARR(1)), JRESREC_WORDS )
           IF ( JERR .NE. JRESREC_WORDS ) THEN
                CALL FERR ( INT2(912), 'Writing residual output', INT2(0), &
     &               INT2(0) )
           ENDIF
      ENDIF
!
      IF ( IAND(IPRES,INT2(1)) .NE. 0  .OR.  RESOUTFILE(1:4) .NE. 'NONE' ) THEN
!
! -------- Mode: printing residuals
!
           STR_DATE = JD_TO_DATE ( FJD+FRACT, -3 )
           IF ( KSPOOL .AND. KFULLOUT ) THEN
                IF ( IOBS .EQ. 1 ) THEN
!
! ------------------ Space to the top of the next page
!
                     IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                          WRITE ( 23, 4004 ) IRNCD
                       ELSE
                          WRITE ( 23, 4005 ) GET_CDATE()
                     END IF
 4004                FORMAT ( 1X/"1Residuals from Run ",I5,"-",I4X/ &
     &                 10X,'Baseline',8X,'Source',6X,'Date       Time ',11X, &
     &                 'Obs Phdel',1X,'PhDel res',1X,'PhDel err',3X, &
     &                 'Obs rate',4X,'Rate res',3X,'Rate err',3X, &
     &                 'elev     frq2-rat QC XS'/ &
     &                 70X,'ps        ps      ps          fs/s     fs/s', &
     &                     '   fs/s  deg')
 4005                FORMAT ( ' '/ &
     &                        '         Residuals from Solve run ', A, &
     &                        '  Format version of 2010.04.10 '/ &
     &                        '         ============================================'// &
     &                        '         Baseline          Source   Date       ', &
     &                        'Time             Obs del Del res   Del er.     ', &
     &                        'Obs rate   Rate res R.err  elev    azim      ', &
     &                        'frq2-rat QC XS    SNR /  SNS_S ',2X &
     &                        'Tau_obs_x       #AmbX   Tau_obs_s        #AmbS   ', &
     &                        'Tau_apriori_clo  Tau_theoretical   ', &
     &                        'Tau_est_contrb     Rate_obs_x           ', &
     &                        'Rate_obs_s           Rate_apriori_clock   ', &
     &                        'Rate_theoretical     Rate_estim_contrib', &
     &                        '     Eff. Dur.    Res_gr_del_X', &
     &                        '      Res_gr_del_S     Gr_Spc_X', &
     &                        '   Gr_Spc_S  USR Ampltude    Phase    Obsind  DS', &
     &                        '  Delay_residual' / &
     &                        69X, 'ps      ps       ps          fs/s    ', &
     &                        'fs/s     fs/s  deg deg deg deg', 36X, &
     &                        'seconds                 seconds                  ', &
     &                        'seconds          seconds           ', &
     &                        'seconds            d/l                  ', &
     &                        'd/l                  d/l                  ', &
     &                        'd/l                  d/l', &
     &                        '                     sec', &
     &                        '         sec               sec', &
     &                        '                 ns         ns', &
     &                        '     d/l d/l         rad', &
     &                        '                     seconds' )
                END IF
!
! ------------- Writing residuals into spoolfile if it was requested
!
                CALL GETENVAR ( 'RESIDUALS_SHORT', STR )
                IF ( STR(1:1) .EQ. 'Y' ) THEN
                     IF ( IDLT == '>' ) THEN
                          DLT_CHR = '@'
                        ELSE
                          DLT_CHR = '!'
                     END IF
                     WRITE ( 23, 124 ) IOBS, DLT_CHR, ISITN_CHR(ISITE(1)), &
     &                                 ISITN_CHR(ISITE(2)), ISTRN_CHR(ISTAR), &
     &                                 (FJD+FRACT-JD_NOM_FIRST)*24.0D0, &
     &                                 1.D9*DOC, LQUAL_CHR, LQUALXS_CHR, &
     &                                 STR_DATE(1:19)
 124                 FORMAT ( I6, 1X, A1, 1X, A, 1X, A, 1X, A, 2X, F7.4, &
     &                        1X, F13.4, ' ns', 1X, A2,'/',A2, 1X, A19 )
                   ELSE
                     STS_STR = '  '
                     IF ( FL_BAS_DESL ) STS_STR(1:1) = 'B'
                     IF ( FL_SOU_DESL ) STS_STR(2:2) = 'S'
                     IF ( FAMB < 1.D-5 ) THEN
                          WRITE ( 23, 120 ) IOBS, IDLT, ISITN_CHR(ISITE(1)), &
     &                        ISITN_CHR(ISITE(2)), ISTRN_CHR(ISTAR), STR_DATE(1:21), &
     &                        FOBS*1.D3, 1.D12*DOC, IPUNC, 1.D12*DOERR, &
     &                        1.D15*ROBS, 1.D15*ROC, IPUNC,  1.D15*RERR, &
     &                        IPELV, IPAZ, COEF_IONO, LQUAL_CHR, LQUALXS_CHR, &
     &                        SNR, SNR_S, &
     &                        TAUGR_OBS_X, NUMAMB, TAUGR_OBS_S, NUMAMB_S, &
     &                        TAU_ACM, TAU_CALC, DD, &
     &                        RATE_OBS_X, RATE_OBS_S, RATE_ACM, RATE_CALC, DR, &
     &                        EFF_DUR(1), RES_GR_DEL(1), RES_GR_DEL(2), &
     &                        FAMB*1.D9, FAMB_S*1.D9, UV_STA_ORDER, AMPL, &
     &                        TOTPH*DEG__TO__RAD, PIND_OBS, STS_STR, DOC
 120                      FORMAT ( I7, A1, 1X, A8, "/", A8, 1X, A, 1X, A, 1X, &
     &                        2(F14.0, F9.1, A1, 1X, F6.0), 4I4, 2X, F9.6, &
     &                        1X, A2, '/', A2, 1X, F6.1, ' / ', F6.1, &
     &                        2X, F16.13, 1X, I5,  2X, F16.13, 2X, I5, &
     &                        2X, F16.13, 1X, F16.13, 2X, F16.13, 2X, &
     &                        1PD20.12, 1X, 1PD20.12, 1X, 1PD20.12, &
     &                        1X, 1PD20.12, 1X, 1PD20.12, ' @@ ', 0PF10.4, &
     &                        2X, 1PD16.8, 2X, 1PD16.8, &
     &                        2X, 0PF9.4, 2X, F9.4, 2X, I2, 2X, &
     &                        F9.7, 2X, F8.5, 2X, I6, 2X, A2, 1X, 1PD20.12 )
                      ELSE
                          WRITE ( 23, 122 ) IOBS, IDLT, ISITN_CHR(ISITE(1)), &
     &                        ISITN_CHR(ISITE(2)), ISTRN_CHR(ISTAR), STR_DATE(1:21), &
     &                        FOBS*1.D3, 1.D12*DOC, IPUNC, 1.D12*DOERR, &
     &                        1.D15*ROBS, 1.D15*ROC, IPUNC, 1.D15*RERR, &
     &                        IPELV, IPAZ, COEF_IONO, LQUAL_CHR, LQUALXS_CHR, &
     &                        SNR, SNR_S, &
     &                        TAUGR_OBS_X, NUMAMB, TAUGR_OBS_S, NUMAMB_S, &
     &                        TAU_ACM, TAU_CALC, DD, &
     &                        RATE_OBS_X, RATE_OBS_S, RATE_ACM, RATE_CALC, DR, &
     &                        EFF_DUR(1), RES_GR_DEL(1), RES_GR_DEL(2), &
     &                        FAMB*1.D9, FAMB_S*1.D9, UV_STA_ORDER, AMPL, &
     &                        TOTPH*DEG__TO__RAD, PIND_OBS, STS_STR, DOC
 122                      FORMAT ( I7, A1, 1X, A8, "/", A8, 1X, A, 1X, A, 1X, &
     &                        2(F14.0, F9.1, A1, 1X, F6.0), 4I4, 2X, F9.6, &
     &                        1X, A2, '/', A2, 1X, F6.1, ' / ', F6.1, &
     &                        2X, F16.13, 1X, I5,  2X, F16.13, 2X, I5, &
     &                        2X, F16.13, 1X, F16.13, 2X, F16.13, 2X, &
     &                        1PD20.12, 1X, 1PD20.12, 1X, 1PD20.12, &
     &                        1X, 1PD20.12, 1X, 1PD20.12, ' @@ ', 0PF10.4, &
     &                        2X, 1PD16.8, 2X, 1PD16.8, &
     &                        2X, 0PF9.3, 2X, F9.3, 2X, I2, 2X, &
     &                        F9.7, 2X, F8.5, 2X, I6, 2X, A2, 1X, 1PD20.12 )
                    END IF
                END IF
           END IF
!
           IF ( K_MN  .AND. .NOT. ( KSPOOL .AND. KFULLOUT ) ) THEN
                IPTR = IPTR +1
                IF ( DABS(FOBS)  > 1.0D7  ) FOBS  = -1.0D6
                IF ( DABS(DOC)   > 1.0D-2 ) DOC   = -1.0D-3
                IF ( DABS(ROC)   > 1.0D-5 ) ROC   = -1.0D-6
                IF ( DABS(DOERR) > 1.0D-2 ) DOERR = -1.0D-3
                IF ( DABS(ROBS)  > 1.0D-5 ) ROBS  = -1.0D-6
                IF ( DABS(RERR)  > 1.0D-5 ) RERR  = -1.0D-6
!
                WRITE ( LBUF(IPTR), FMT=130 ) IOBS, IDLT, ISITN_CHR(ISITE(1)), &
     &                  ISITN_CHR(ISITE(2)), ISTRN_CHR(ISTAR), STR_DATE(1:21), &
     &                  IDNINT(FOBS*1.D3), IDNINT(1.D12*DOC), IPUNC, &
     &                  IDNINT(1.D12*DOERR), IDNINT(1.D15*ROBS), &
     &                  IDNINT(1.D15*ROC), IPUNC, IDNINT(1.D15*RERR)
 130            FORMAT ( I7, A1, 1X, A8, "/", A8, 1X, A8, 1X, A, 1X, &
     &                   2(I13, 1X, I6, A1, 1X, I3) )
!
                CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                CALL NL_MN()
              ELSE IF ( K_MN .AND. IOBS .EQ. 1 ) THEN
                IPTR = IPTR +1
                CALL CLRCH ( LBUF(IPTR) )
                CALL ADDSTR_F ( LBUF(1:1) )
                CALL NL_MN()
!
                IPTR = IPTR +1
                LBUF(IPTR) = '###   Residuals are written in spool file   ###'
                CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                CALL NL_MN()
           END IF
      END IF
!
      IF ( FL_DEBUG_SBA .AND. FL_USED ) THEN
           IF ( BTEST ( DTEC_FLG, DTHL__STS ) ) THEN
                WRITE ( 6, 140 ) NOBS, 'DUAL', ISITN_CHR(ISITE(1))//' / '//ISITN_CHR(ISITE(2)), &
     &                           ISTRN_CHR(ISTAR), 1.D12*DOC, 1.D12*COR_TAU
 140            FORMAT ( 'CRES ', I5, 1X, A, 1X, 'Sta: ', A, ' Src: ', A, ' Res: ', F8.1, &
     &                   ' Cor_tau: ', F8.1, ' ps' )
              ELSE IF ( BTEST ( DTEC_FLG, DTH__STS ) ) THEN
                WRITE ( 6, 150 ) NOBS, 'H   ', ISITN_CHR(ISITE(1))//' / '//ISITN_CHR(ISITE(2)), &
     &                           ISTRN_CHR(ISTAR), 1.D12*DOC, 1.D12*COR_TAU, 1.D12*DEL_BIAS_UL, &
     &                           TEC_APR, DTEC_ADJ
 150            FORMAT ( 'CRES ', I5, 1X, A, 1X, 'Sta: ', A, ' Src: ', A, ' Res: ', F8.1, &
     &                   ' Cor_tau: ', F8.1, ' ps  Del_bias: ', F9.1, &
     &                   ' ps Tec_apr: ', F7.2, 1X, F7.2, ' Dtec_adj: ', F7.2 )
              ELSE IF ( BTEST ( DTEC_FLG, DTL__STS ) ) THEN
                WRITE ( 6, 150 ) NOBS, 'L   ', ISITN_CHR(ISITE(1))//' / '//ISITN_CHR(ISITE(2)), &
     &                           ISTRN_CHR(ISTAR), 1.D12*DOC, 1.D12*COR_TAU, 1.D12*DEL_BIAS_UL, &
     &                           TEC_APR, DTEC_ADJ
              ELSE
                WRITE ( 6, 160 ) NOBS, ISITN_CHR(ISITE(1))//' / '//ISITN_CHR(ISITE(2)), ISTRN_CHR(ISTAR), &
     &                          'Undefined DTEC status ', 1.D12*DOC, 1.D12*COR_TAU, &
     &                           BTEST ( AUTO_SUP, INT4(FURE__SPS) ), &
     &                           BTEST ( USER_SUP, INT4(IDATYP) ), &
     &                           META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS ), FL_USED
           END IF
         ELSE IF ( FL_DEBUG_SBA .AND. .NOT. FL_USED ) THEN
           WRITE ( 6, 160 ) NOBS, ISITN_CHR(ISITE(1))//' / '//ISITN_CHR(ISITE(2)), ISTRN_CHR(ISTAR), &
     &                      'DESELECTED', 0.0, 0.0,              &
     &                      BTEST ( AUTO_SUP, INT4(FURE__SPS) ), &
     &                      BTEST ( USER_SUP, INT4(IDATYP) ),    &
                            META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS ), FL_USED
 160       FORMAT ( 'CRES ', I5, 1X, ' Sta: ', A, ' Src: ', A, &
     &              ' status= ', A, ' Res: ', F12.1, &
     &              ' Cor_tau= ', F8.1, ' ps  Flag: ', L1, ' User: ', L1, ' fl_used: ', L1, 1X, L1 )
      END IF
!
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) .AND. &
     &     ( IUNWP .EQ. 8  .OR. IUNW .EQ. 8 )       ) THEN
!
! -------- Special trick to alleviate work of CNPLT: phase delay residual
! -------- for the obervation with no S-band counterpart sets forcibly to zero
!
           DOC = 0.0D0
      END IF
!
! --- Preparing record for writing to binary residual file
!
      IRSITE(1)      = ISITE(1)
      IRSITE(2)      = ISITE(2)
      IRSTAR         = ISTAR
      RDOC           = 1.D9*DOC
      RROC           = 1.D12*ROC
      RDERR          = 1.D9*DOERR
      RRERR          = 1.D12*RERR
      RFJD           = FJD
      RFRCT          = FRACT
      RELEV(1)       = ELEV(1)*180./PI__NUM
      RELEV(2)       = ELEV(2)*180./PI__NUM
      TAU_C          = TAU_CALC
      RATE_C         = RATE_CALC
      RFAMB          = FAMB
      NAMB           = NUMAMB
      IRPNTR         = IPNTR
      SUPSTAT_RES(1) = SUPSTAT(1)
      SUPSTAT_RES(2) = SUPSTAT(2)
      UACSUP_RES     = UACSUP
      AUTO_SUP_RES   = AUTO_SUP
      USER_SUP_RES   = USER_SUP
      USER_REC_RES   = USER_REC
!
! --- Calculate post_fit error. Taking the square root takes care of the rare
! --- cases when post_fit_residual is 0. Due to roundoff error sometimes
! --- got NaN when doing square root.
!
      PDERR = SQRT ( ABS ( RDERR**2 - POST_FIT_DEL_COR*1.D18 ) )
      PRERR = SQRT ( ABS ( RRERR**2 - POST_FIT_RAT_COR*1.D24 ) )
!
!
      IF ( KBIT ( PRE_IP(3), INT2(12)) .OR.  .NOT. KBATCH ) THEN
           CALL USE_RESFIL ( IOBS, 'W' )
      END IF
 1000 CONTINUE ! end of cycle with NOBS
!
      IF ( EDC_USE .NE. EDC__UNDF ) THEN
!
! -------- Release memory grabbed by EDC and initialize EDC
!
           CALL ERR_PASS ( IUER, IER )
           CALL EDC_QUIT ( EDC, IER  )
      END IF
!
! === End main loop over the observations
!     ===================================
!
      IF ( K_MN ) THEN
           CALL NL_MN()
           CALL NL_MN()
      ENDIF
      IF ( KUSER_PART .AND. ( ARC_USER_PART > 0 .OR.  NUM_USER_PART > 0 ) ) THEN
           CALL BIN_CLOSE ( FNAME, FILDES )
      END IF
      IF ( RESOUTFILE(1:4) .NE. 'NONE'  .AND. KBATCH ) THEN
           CALL BIN_CLOSE ( FNAMEB, FILDES2 )
      ENDIF
!
! --- Calculate average and rms for humidity for each station
!
      DO I=1,NUMSTA
         IF ( NHUM(I).GT.0 ) THEN
              AVG_HUMID(I) = AVG_HUMID(I) / NHUM(I)
              DO J=1,NHUM(I)
                 RMS_HUMID(I) = RMS_HUMID(I) + ( HUM(J,I) - AVG_HUMID(I) )**2
              ENDDO
              RMS_HUMID(I) = (RMS_HUMID(I) / NHUM(I))**0.5
           ELSE
              AVG_HUMID(I) = 0.D0
              RMS_HUMID(I) = 0.D0
         ENDIF
      ENDDO
!
      IF ( KSPOOL .AND. KFULLOUT ) THEN
!
! -------- Print out met statistics to spool file
!
           WRITE ( 23, '(/"  Met Statistics:")')
           WRITE ( 23, '(20X,"Temperature      Pressure   ","     Humidity")')
           WRITE ( 23, '("   Station",9X,"average   rms   average   rms", &
     &                   "   average   rms")')
           CALL GETCARD ( IDB, 'MDAT', INT2(1), JBUF, IERR )
           READ ( JBUF, '(5X,I3)', IOSTAT=IOS ) NMET
           CALL FERR ( INT2(IOS), "Reading NAMFIL MDAT card", INT2(0), INT2(0) )
!
           DO I=1,NMET
              CALL GETCARD ( IDB, 'MDAT', INT2(0), JBUF, IERR )
              IF ( JBUF(51:56) == '******' ) JBUF(16:21) = ' 999.9'
              IF ( JBUF(51:56) == '******' ) JBUF(23:28) = ' 999.9'
              IF ( JBUF(51:56) == '******' ) JBUF(30:39) = ' 999.9'
              IF ( JBUF(51:56) == '******' ) JBUF(37:42) = ' 999.9'
              IF ( JBUF(51:56) == '******' ) JBUF(44:49) = ' 999.9'
              IF ( JBUF(51:56) == '******' ) JBUF(51:56) = ' 999.9'
              READ ( JBUF, '(5X,A8,1X,8F7.1)', IOSTAT=IOS) &
     &               MET_SITE, ( METSTAT(J), J=1,8 )
              CALL FERR ( INT2(IOS), "Reading NAMFIL MDAT card", INT2(0), &
     &             INT2(0) )
              WRITE ( 23, '(3X,A8,"  MET ",6F8.1)' ) MET_SITE,METSTAT(3), &
     &        METSTAT(4), METSTAT(7), METSTAT(8), AVG_HUMID(I), RMS_HUMID(I)
           ENDDO
      ENDIF
!
      IF ( K_MN ) THEN
!
! -------- Printout meteo statistic on screen
!
           IPTR = IPTR +1
           WRITE ( LBUF(IPTR), '(1x)' )
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL NL_MN()
           IPTR = IPTR +1
           WRITE ( LBUF(IPTR), '("  Met Statistics:")')
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL NL_MN()
           IPTR = IPTR +1
           WRITE ( LBUF(IPTR), '(20X,"Temperature      Pressure   ", &
     &                               "     Humidity")')
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL NL_MN()
           IPTR=IPTR+1
           LBUF(IPTR) = "   Station         average   rms   average   rms"// &
     &                  "   average   rms"
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL NL_MN()
           CALL GETCARD ( IDB, 'MDAT', INT2(1), JBUF, IERR )
           READ ( JBUF, '(5X,I3)',IOSTAT=IOS ) NMET
           CALL FERR ( INT2(IOS), "Reading NAMFIL MDAT card", INT2(0), INT2(0) )
           DO I=1,NMET
              CALL GETCARD ( IDB, 'MDAT', INT2(0), JBUF, IERR )
              IF ( JBUF(16:21) == '******' ) JBUF(16:21) = '  0.0 '
              IF ( JBUF(23:28) == '******' ) JBUF(23:28) = '  0.0 '
              IF ( JBUF(30:35) == '******' ) JBUF(30:35) = '  0.0 '
              IF ( JBUF(37:42) == '******' ) JBUF(37:42) = '  0.0 '
              IF ( JBUF(44:49) == '******' ) JBUF(44:49) = '  0.0 '
              IF ( JBUF(51:56) == '******' ) JBUF(51:56) = '  0.0 '
              IF ( JBUF(58:63) == '******' ) JBUF(58:63) = '  0.0 '
              IF ( JBUF(65:70) == '******' ) JBUF(65:70) = '  0.0 '
              READ ( JBUF, '(5X,A8,1X,8F7.1)', IOSTAT=IOS) &
     &               MET_SITE,(METSTAT(J),J=1,8)
              CALL FERR ( INT2(IOS), "Reading NAMFIL MDAT card", INT2(0), &
     &                    INT2(0) )
              IPTR = IPTR +1
              WRITE ( LBUF(IPTR), '(3X,A8,"  MET ",6F8.1)' ) MET_SITE, &
     &                METSTAT(3), METSTAT(4), METSTAT(7), &
     &                METSTAT(8), AVG_HUMID(I), RMS_HUMID(I)
              CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
              CALL NL_MN()
           ENDDO
      ENDIF
 1009 CONTINUE
      IDBGN = IDBEND(IDB) + 1
      IF ( APRIORI_ZENDEL ) THEN
           CLOSE ( UNIT=LUN_AZ )
      END IF
!
      CALL GETENVAR ( 'NO_MECHI', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR == 'YES' ) THEN
           FL_NOMECHI = .TRUE.
         ELSE 
           FL_NOMECHI = .FALSE.
      END IF
      IF ( ISLTY2 .EQ. 'I'  .AND.  CNSTROBJ%N_EQUAT .GT. 0  .AND. .NOT. FL_NOMECHI ) THEN  ! only in INDEPENDENT solution type
!
! -------- Compute mathematical expectation of the sum of squares weighted
! -------- residuals
!
           CALL ERR_PASS ( IUER, IER )
           LEN_WEI_CNS = 8*(CNSTROBJ%N_EQUAT*(CNSTROBJ%N_EQUAT+1))/2
           CALL GRAB_MEM ( IER, MEM_LEN, MEM_ADR, 1, LEN_WEI_CNS, &
     &                     ADR_WEI_CNS )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( MEM_LEN, STR )
                CALL ERR_LOG ( 8945, IUER, 'SECND', 'Error in an '// &
     &              'attempt to grab '//STR(1:I_LEN(STR))//' bytes of '// &
     &              'dynamic memory' )
                RETURN
           END IF
           ME_CHI = COMPUTE_MECHI ( NC, NPARAM, ARR(JA), CNSTROBJ, &
     &                              %VAL(ADR_WEI_CNS)  )
           CALL FREE_MEM ( MEM_ADR )
         ELSE IF ( ISLTY2 .EQ. 'I'  .AND.  ( CNSTROBJ%N_EQUAT .LE. 0 .OR. FL_NOMECHI ) ) THEN
!
! -------- No constraints
!
           ME_CHI = NC - NPARAM
         ELSE
           ME_CHI = 0.0D0
      END IF
!
! --- Compute weighted mean dates
!
      IF ( WW_ALL .GT. 1.D-20 ) THEN
!
! -------- over all observations
!
           JDATE_ALL_MID = JDATE_ALL_MID/WW_ALL + JDATE_ALL_BEG
         ELSE
           JDATE_ALL_MID = (JDATE_ALL_END + JDATE_ALL_BEG)/2.0D0
      END IF
!
! --- ... and over participating stations
!
      DO 4110 J11=1,NUMSTA
         IF ( WW_STA(J11) .GT. 1.D0/BIG_VALUE ) THEN
              JDATE_STA_MID(J11) = JDATE_STA_MID(J11)/WW_STA(J11) + JDATE_ALL_BEG
            ELSE
              JDATE_STA_MID(J11) = JDATE_ALL_MID
         END IF
 4110 CONTINUE
!
! --- Save them in glbc4
!
      CALL USE_GLBFIL_4 ( 'OWC' )
!
 1010 CONTINUE
      IF ( IEOPL.NE.0 ) EOPLOT_CT = EOPLOT_CT-1
!
! --- Finish atmosphere path delay statistics
!
      DO 4120 J12=1,NUMSTA
         IF ( NAT(J12) > 1 ) THEN
              IF ( .NOT. IS_R8_NAN(TZD_AVG(J12)) .AND. .NOT. IS_R8_NAN(TZD_RMS(J12)) ) THEN
                   TZD_AVG(J12) = TZD_AVG(J12)/NAT(J12)
                   TZD_RMS(J12) = DSQRT ( MAX( TZD_RMS(J12)/NAT(J12) - TZD_AVG(J12)**2, 1.D-30) )
                 ELSE 
                   TZD_AVG(J12) = 0.0D0
                   TZD_RMS(J12) = 0.0D0
              END IF
!
              IF ( .NOT. IS_R8_NAN(WZD_AVG(J12)) .AND. .NOT. IS_R8_NAN(WZD_RMS(J12)) ) THEN
                   WZD_AVG(J12) = WZD_AVG(J12)/NAT(J12)
                   WZD_RMS(J12) = DSQRT ( MAX( WZD_RMS(J12)/NAT(J12) - WZD_AVG(J12)**2, 1.D-30) )
                 ELSE
                   WZD_AVG(J12) = 0.0D0
                   WZD_RMS(J12) = 0.0D0
              END IF
!
              IF ( .NOT. IS_R8_NAN(STM_AVG(J12)) .AND. .NOT. IS_R8_NAN(STM_RMS(J12)) ) THEN
                   STM_AVG(J12) = STM_AVG(J12)/NAT(J12)
                   STM_RMS(J12) = DSQRT ( MAX( STM_RMS(J12)/NAT(J12) - STM_AVG(J12)**2, 1.D-30) )
                 ELSE
                   STM_AVG(J12) = 0.0D0
                   STM_RMS(J12) = 0.0D0
              END IF
!
              IF ( .NOT. IS_R8_NAN(SPR_AVG(J12)) .AND. .NOT. IS_R8_NAN(SPR_RMS(J12)) ) THEN
                   SPR_AVG(J12) = SPR_AVG(J12)/NAT(J12)
                   SPR_RMS(J12) = DSQRT ( MAX (SPR_RMS(J12)/NAT(J12) - SPR_AVG(J12)**2, 1.D-30) )
                 ELSE
                   SPR_AVG(J12) = 0.0D0
                   SPR_RMS(J12) = 0.0D0
              END IF
         ENDIF
 4120 CONTINUE 
!
! --- End of work
!
      IF ( FAST_DBG .EQ. F__APP  .OR.   FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, 240 )  nparam
 240       format ( 1x,' Farwell CRES. nparam= ', I6 )
      END IF
      IF ( TPD_USE_FLAG == TPD__USE  .OR.  TPD_USE_FLAG == TPD__UPD ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL TPD_QUIT ( TPD, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8946, IUER, 'SECND', 'Error in an '// &
     &              'attempt to release memory grabed by TPD while the '// &
     &              'experiment '//B3DOBJ%DBNAME_MES//' was beging processed' )
                RETURN
           END IF
      END IF
      IF ( ILEN(VTD_CONF_USE) > 0 .AND. .NOT. FL_TPD_READ ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_QUIT ( %VAL(VTD_ADR), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8947, IUER, 'SECND', 'Error in VTD_QUIT' )
                RETURN
           END IF
           IF ( MEM_LEN_VTD .NE. 0 ) THEN
                CALL FREE ( MEM_ADR_VTD )
                MEM_LEN = 0
                MEM_ADR = 0
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   SECND  !#!#
