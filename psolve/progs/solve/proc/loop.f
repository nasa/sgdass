      SUBROUTINE LOOP ( B3DOBJ, B1B3DOBJ, ARR, EQUOBS, SNGCHK_CMP, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  LOOP PROGRAM SPECIFICATION
!
! 1.1 This is the main loop of program PROC.
!
! 1.2 REFERENCES:
!
! 2.  LOOP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: none
!
! 2.3 OUTPUT Variables: none
      REAL*8 ARR(*)
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbcm.i'
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'oborg.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'obser.i'
      INCLUDE 'vtd.i'
      INCLUDE 'edc.i'
      INCLUDE 'iono_vmod.i'
      INCLUDE 'equobs.i'
      COMMON / DERCM / DERIV, TDPART, TRPART
      TYPE     ( VTD__OBS_TYPE ) :: OBS_TYP
      TYPE     ( VTD__TYPE     ),   POINTER :: VTD_PTR
      TYPE     ( EDC__TYPE     ) :: EDC
      TYPE     ( TPD__TYPE     ) :: TPD
      TYPE     ( TCN__TYPE     ) :: TCN(M__TCN)
      TYPE     ( IONOV__STRU   ) :: IONOV
      TYPE     ( NERS__TYPE    ) :: NERS
      TYPE     ( PSOLVE__EQUOBS_TYPE ) :: EQUOBS
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: proc
!       CALLED SUBROUTINES: nrmeq
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4  JA, JB
      INTEGER*8  NELEM
!
      REAL*8    DERIV(M_GPA,2),       TDPART(M_GPA), TRPART(M_GPA)
      REAL*8    LATS(MAX_ARC_STA),      HEIGHTS(MAX_ARC_STA)
      REAL*8    BARO_CALS(MAX_ARC_STA), BARO_HEIGHTS(MAX_ARC_STA)
      REAL*8    APP(2,2), ET(2,MAX_ARC_BSL), SE(MAX_ARC_STA), SS(MAX_ARC_SRC)
      INTEGER*2 AX_TYPES(MAX_ARC_STA)
      INTEGER*2 I, J, IDB, JSITN(4,MAX_ARC_STA)
      INTEGER*2 JSITI(MAX_ARC_STA), JCAPPL(MAX_ARC_STA)
      INTEGER*2 ITT(MAX_ARC_STA), ITTB(MAX_ARC_BSL)
      INTEGER*2 JCAVAL(MAX_ARC_STA), NOGOOD
      INTEGER*4 NOBS, KOBS, IDBGN, IDEND
      REAL*8    AX_OFFS(MAX_ARC_STA)
      REAL*8    DERR_RAW, RERR_RAW, DPHER_RAW
      REAL*8    DOC, ROC, DOERR, AVG_ATM(4,MAX_ARC_STA)
      LOGICAL*2 KBIT, K_MN, POLYONLY
      LOGICAL*1 FL_VTD_IONO, FILE_IONO_EX, FL_OBSFIL_UPDATE 
      INTEGER*4 IOS, IS
      CHARACTER FNAME_PART*(NAME_SIZE), FNAMED*(NAME_SIZE)
      INTEGER*4 FILDES_PART, ITMP1, ITMP2, ITMP3, ITMP4, L_TCN
      CHARACTER EDBSL*17, COMPBSL*17, EDSRC*8, COMPSRC*8, CBUF*80, &
     &          IORE*3, JBUF*70
      INTEGER*2 EDYR, EDMN, EDDAY, EDHR, EDMIN, IY, IM, ID, IHR, IMIN, ITIME
      INTEGER*2 LDBNAM(5,15), IDBV(15)
      INTEGER*4 IDBE(15), OBCOUNTR, EVINT4, EVSTART4, RAN_SEED_I4, IND_OBS, &
     &          IL, NE, NA, N_WEI, N_DTEC, N_EXT_ERR, MJD_1ST
      CHARACTER CDBNAM(15)*10
      EQUIVALENCE (CDBNAM,LDBNAM(1,1))
      COMMON     / PARTFILE / FNAME_PART, FILDES_PART
!
      CHARACTER   BUF_AOC(MAX_OBS)*80, BUF_EDIT(MAX_OBS)*16, BUF_ADDW(MAX_OBS)*64, &
     &            BUF_DTEC(MAX_OBS)*256, BUF_EXT_ERR(MAX_OBS)*64, &
     &            FILOUT_EDIT*128, FILOUT_ADDW*128, FILOUT_SCAW*128
      INTEGER*2   OBCAPL, MCAPL, ICONT_I2, IERR_I2
      INTEGER*4   L_ACM
      REAL*8      CLOOF_ACM(M_ACM), CLODR_ACM(M_ACM)
      REAL*8      LAT_GCN(2), LAT_GDT(2), LONG(2), IONO_GPS, IONO_VLBI, VAL_R8, &
     &            UV_PROC, LAMBDA
!
      INTEGER*2   JCAFFL(7,MAX_ARC_STA), NFCAL, NAMSTA
      INTEGER*4   NPARAM_OLD
      CHARACTER   FCAL_NAMES(112)*8
      CHARACTER   VTD_CONF_USE*128, BUFSTR*80, STA_NAM(2)*8, &
     &            STAT_ACM(M_ACM)*8, STR*80, STR1*80, STR2*80
!
      LOGICAL*2  FL_11, FL_WRONG_FREQ_PRINT
      LOGICAL*1  FL_AOC, FL_EDIT, FL_ADDW, FL_ADDW_BW, FL_ADDW_IONO, FL_DTEC, &
     &           FL_EXT_ERR, FL_DEBUG_SBA, FL_SUP(MAX_OBS)
      REAL*8     TAU_CALC,      RATE_CALC,     COR_TAU,       COR_RATE, &
     &           ADDERR_GR_TAU, ADDERR_PH_TAU, ADDERR_RATE, &
     &           TAUGR_OBS_X,   TAUGR_OBS_S,   TAUPH_OBS_X,   TAUPH_OBS_S, &
     &           TAUSB_OBS_X,   TAUSB_OBS_S,   TAUGR_ERR_X,   TAUGR_ERR_S, &
     &           TAUPH_ERR_X,   TAUPH_ERR_S,   TAUSB_ERR_X,   TAUSB_ERR_S, &
     &           RATE_OBS_X,    RATE_OBS_S,    RATE_ERR_X,    RATE_ERR_S, &
     &           FREQ_GR_X,     FREQ_GR_S,     FREQ_PH_X,     FREQ_PH_S, &
     &           FREQ_RATE_X,   FREQ_RATE_S,   TAU_OC,        RATE_OC, &
     &           TAU_E, RATE_E, TAUGR_X_ERR_ORIG, TAUGR_S_ERR_ORIG, TAU_REWEI, &
     &           ADDW_EXT(MAX_OBS), EXT_ERR(MAX_OBS), ADDW_FRQ, ADDW_SES_SCL, &
     &           TAI_1ST, DTEC_GPS_EXT(2,MAX_OBS), &
     &           DTEC_ADJ_EXT(2,MAX_OBS), DTEC_ERR_EXT(MAX_OBS), &
     &           DEL_BIAS_UL_EXT(MAX_OBS), DTEC_VMG(MAX_OBS), &
     &           DTEC_RES(MAX_OBS), &
     &           VIONO_ERR(MAX_OBS), TAU_O, TAU_IGX, TAU_IGS, FREQ_K, &
     &           TIM_UPWEI_BEG, TIM_UPWEI_END
      INTEGER*4  ITIM_UPWEI_BEG, ITIM_UPWEI_END
      REAL*8     IONO_OBS_X, IONO_APR_X, IONO_MOD_X, IONO_RES_X, IONO_RRS_X, &
     &           IONO_ERR_X
      REAL*8     DT_SAVE, RT_SAVE, DERR_SAVE, RERR_SAVE, DPHER_SAVE, &
     &           WW_STA(MAX_ARC_STA), WW_ALL, BIG_VALUE, TMIN, TT2, SIG
      REAL*8     DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), DELAY_THR, &
     &           RATE_THR, PRES_VAL, TEMP_VAL, TAI_OBS, BAS_LEN, &
     &           IONO_ZEN_AVR(2), IONO_ZEN_COV(3), DEL_GPS_IONO(2), &
     &           AOC_DEL(MAX_OBS), IONO_FX, IONO_FS, VTD_IONO_SCALE
      INTEGER*2   N_VAR, J_VAR, JJ_VAR
      CHARACTER, EXTERNAL :: JD_TO_DATE*23
      PARAMETER  ( BIG_VALUE = 1.D10 )
      INTEGER*8        MEM_LEN
      ADDRESS__TYPE :: MEM_ADR
      INTEGER*4  VERB_SNGCHK, SNGCHK_ACTION_SAVED, SIZEOF_VTD
!
      INCLUDE 'flyby.i'
!      integer*4  np
!      real*8     t8(32000), x8(32000), ww
!
! 4.  HISTORY
!  WHO  WHEN     WHAT
!  CEK  89.02.07 Elevation cutoff by station and WVR mask logic added.
!  MWH  90.02.26 Modify to include Lanyi model
!  MWH  91.05.24 Add flag to indicate only polynomial clock parameters
!                    (i.e. no stochastic)
!  mwh  94.02.01 Implement dynamic memory allocation for normal matrix
!  kdb  95.08.31 Add atmospheric turbulence rate constant and mapping function
!                    parameter error delay and rate constants.
!  kdb  951207   Integer*4 number of observations.
!  pet  970128   Substantial pieces of code handling implementation B3D
!                    algorithm added. Comments improved
!  pet  970129   Hatch bypassing clock breaks added
!  pet  970205   Trap for sessions with wrong order of observations
!  pet  970226   add stuff for B1B3D algorithm implementation
!  pet  970304   add keeping theoreticals modified by flyby. It is done for not
!                calling flyby in CRES
!  pet  970307   fixed bug: in some cases B3DOBJ was uninitialized
!  pet  970429   Added interpolation high frequency EOP.
!  pet  970604   Fixed bug for the case when several databases analyzed.
!  pet  970606   Changed logic for saving calibrations
!  pet  970712   Suppressed printout when it called from REWAY
!  pet  970803   Changed the order of test to reject the observation in order
!                to force LOOP to check the order of the observations even
!                in for the case when the unused observations are in wrong order
!  pet  970912   Added recalculation of NUMSCA in loop. If new NUMSCA differs
!                from the old NUMSCA from SOCOM then NUMSCA will be updated in
!                SOCOM area.
!  pet  971006   Added routine DBG_WRITE -- in order to be able to printout
!                some debug information in FAST_MODE NONE
!  pet  971124   Accommodated changes in the set of formal parameters for NCORT
!  pet  971204   Added logic for splitting baseline deselection on phase delay
!                and group delay solution type
!  pet  980203   Substituted hard-coded test of solution type by DATYP_INQ
!  pet  980219   Removed call of socal and put instead of it calls get_calib and
!                make_oc -- they support extended list of solution type.
!  pet  980428   Changed logic for bypassing deselected observations: added
!                calls of SUPSTAT_SET and SUPR_INQ for obtaining status
!                of the observation in the solution
!  pet  980514   Important bug fixed: procedure FAST_BYPASS was moved from loop
!                to proc. The true is that FAST_BYPASS changes fast_mode if
!                session is not eligible for fast algorithm. But dynamic memory
!                is allocated in according with USED value of fast_mode. For
!                this reason FAST_BYPASS should be located before grabbing
!                dynamic memory
!  pet  980707   Added logic for singularity of the normal system checking
!  pet  980720   Corrected a bug: normal matrix and normal vector were not
!                zeroed after reparameterization in the previous versions
!  pet  980913   Fixed a minor bug: user partial file should be opened the
!                next time if normal matrix is recomputed after failure
!                singularity check and consecutive reparameterization
!  pet  980920   Added calculation actual and nominal duration of the session
!  pet  980921   Corrected S-band related bug. Added a special treatment of
!                the formal error of ionosphere free linear combination for
!                the case when effective frequency corresponds to S-band
!  pet  981230   Corrected a bug related with losing position in user partial
!                file due to skipping bad observations.
!  pet  981230   Corrected a bug: the previous version didn't suport in F__NON
!                mode the case when the number of parameters increased in
!                result of reparameterization.
!  pet  990105   Rewrote error handler. Added a formal argument IUER.
!  pet  990108   Added a call of FLYBY_MAP_INIT
!  pet  990129   Added a check whether effective frequency is Not-A-Number
!  pet  990405   Added call TIM_GET
!  pet  990406   Set verbosity level of SNGCHK in dependence of global variable
!                G_WARNING from glbc4.i
!  pet  990417   Corrected a bug: disabeled reparameterization in global
!                solution of non-fast mode. The reason: there is no way to
!                pass information about changes in the list of
!                stations/sources/baselines to BACK in NON-fast mode.
!  pet  990421   Forced loop to bypass calibration and computation of
!                atmosphere partials if there were no fringes at least at
!                one bad (fuality codes are 0 or letter) to prevent error
!                messages in attempt to handle observations with negative
!                elevation angles
!  pet  1999.05.03   Moved zeroing B3DOBJ from loop to proc_head and added
!                    initialization of B3DOBJ.R_SOU, B3DOBJ.R_STA, B3DOBJ.R_BAS
!  pet  1999.05.21   Corrected a bug: moved a bit up initialization of
!                    B3DOBJ.R_SOU, B3DOBJ.R_STA, B3DOBJ.R_BAS
!  pet  1999.06.08   Changed reaction on NaN for frequencies. Changed order of
!                    operations near call make_oc.
!  pet  1999.11.17   Updated the list of actual parameters for NCORT and
!                    GET_CALIB
!  pet  2001.05.01   Added code for correction formal delay uncertainty:
!                    if it is out of range [ TAU_ERR__BAD, TAU_ERR__TINY ]
!                    it is set to boundary of the range
!  pet  2002.05.03   Added code for computing actual start and as well
!                    as weighted mean epoch of the session and for every
!                    participating station
!  pet  2002.05.06   Added computation of the weighted sum of o-c for delay and
!                    delay rate
!  pet  2002.06.03   Added writing computed weighted mean epochs to glbc4
!  pet  2002.06.11   Fixed the bug in the code above
!  pet  2003.08.06   Added a bypass of a bug in HP Fortran90 2.5.1 compiler
!  pet  2003.08.18   Added variable USE_EDIT in order to have an ability to
!                    block reading EDxx file when it is not needed
!  pet  2003.08.19   Fixed a bug: the privious version tried to work with
!                    unitialized variables in the case if the first observation
!                    of the sessions is non-detection. In rary cases it
!                    may result to float point execption and abnormal
!                    termination
!  pet  2007.02.14   loop now generats a new "Delay Output" if SOLVE_DEBUG
!                    environment variable is set to 7.
!  pet  2007.02.14   Added supoport of a new kludge option. If SOLVE_DEBUG
!                    environment variable has the first character 2, &
!                    then the rest of the environment variable as
!                    treated as a command for substitutin differnet delay type
!                    for the spedicied source.
!                    Example:
!                    setenv SOLVE_DEBUG "2 0501+279 SB_X-->GR_X  SB_S-->GR_S"
!                    causes SB_X to be temporarily copied in to GR_X and &
!                           SB_S to be temporarily copied in to GR_S for
!                           all observations of source 0501+279. &
!                    Recognied words: SB_S, SB_X for single band delays and
!                                     GR_S, GR_X for group delays
!
!   pet  2007.10.26  Added support of exteinral decimation files
!
!   pet  2007.11.08  Added support of external theoerical path delay files
!                    If SOLVE_DEBUG(1:1) == 'I' and suppression method is META,
!                    then suppression status is reset to "nothing is suppressed"
!   pet  2010.02.28  Added support of the kludge variable SOLVE_SNR_MIN
!   pet  2010.10.10  Added support of SOLVE_DEBUG 'GET_IONO' -- printing &
!                    information about ionosphere path delay computed &
!                    using GPS TEC maps
!   pet  2010.10.21  Added support of TCN -- Tec Noise File -- additional &
!                    noise added in quadrature to reciprocal weights in order
!                    to account for errors in a GPS TEC model
!   pet  2010.11.02  Added support of SOLVE_DEBUG 'DISCARD_S' -- S-band &
!                    information is set to zero
!
!   pet  2012.01.23  Added support of variable SOLVE_DIR_DEBUG
!
!   pet  2013.06.11  Added support of update of "Low SNR" suppression status on the fly &
!                    in non-interactive solution
!   pet  2014.09.09  Added support of apriori cloc model for a case when path &
!                    delay is computed on the fly with VTD.
!   pet  2016.03.26  Added support of value VERB of the environment variable TPD_USE &
!                    that generates debugging output
!   pet  2016.11.23  Added support of SOLVE_DEBUG 'PUT_IONO' -- printing
!                    information related to dual-band observations
!   pet  2016.12.26  Added support of generation of the ionosphere path delay template
!   pet  2016.12.31  Added support of DUAL observation type
!   pet  2018.08.26  Changed the output in GET_IONO debugging mode
!   pet  2018.09.08  Added support of IONO_ERR_FCT variable for inflating group delay
!                    uncertainty in order to accommodate ionosphere path delay errors
!   pet  2018.12.23  Added support of ERR_FUDGE_FACTOR defined as an ARC option
!   pet  2019.10.18  Made changes in order to support new interface to GET_UTC_M_TAI
!   pet  2020.04.27  Implemented support of an edit file with external suppression flags
!   pet  2020.07.14  Added support of the external additive weight correction passed via 
!                    file defined as session-specific option.
!   pet  2020.07.15  Added support of the environement variables SOLVE_EXPORT and
!                    SOLVE_EXPORT_DIR
!
!                    If SOLVE_EXPORT contains word edit, then the editing file is 
!                    written in the output file $SOLVE_EXPORT_DIR/xxxxxxxxxx_edit.txt,
!                    where xxxxxxxxxx is experiment name. The editing file contains
!                    the indices of suppress observations.
!
!                    If SOLVE_EXPORT contains word addw, then the additive weight file 
!                    is written in the output file $SOLVE_EXPORT_DIR/xxxxxxxxxx_addw.txt,
!                    where xxxxxxxxxx is experiment name. The additive weight file 
!                    contains the indices and additive weight correction that is
!                    supposed to be added in quadrature to the observation uncertainty.
!
!                    If SOLVE_EXPORT_DIR is not defined, then /tmp is used as default.
!
!   pet  2021.03.17  Added support of exteral apriori observation file
!   pet  2021.12.30  Disabled support of ERR_FUDGE_FACTOR and replaced it with
!                    support of SESS_REWEI_SCALE and SESS_REWEI_QUADR parameters
!                    defined an ARC option
!   pet  2021.12.31  Added support of external dTEC files that define dTEC, dTEC error and
!                    ionosphere-free bias of the group delay at the upper band with
!                    respect to the lower band
!   pet  2022.02.14  Added support of fused data type
!   pet  2022.06.08  Added support of adjustement to ionospheric path delay 
!   pet  2022.06.26  Added support of TEC_SCAL and TEC_BIAS
!   pet  2022.12.06  Added logic for support of collecting information about the ionospheric
!                    path delay and ionospheric bias estimation
!   pet  2023.01.06  Added support of environment variable STR_DTEC_SBA_ERR_SCL 
!                    that sets the use of single band dTEC adgjusments and scales 
!                    its errors
!   pet  2023.02.05  Added support of the output of information in the debugging mode
!                    SCA_WEI_SUP
!   pet  2023.02.10  Added support of external error file
!   pet  2024.11.25  Disabled spiort of debugging mode SCA_WEI_SUP and replaced it &
!                    with a support of environment variable SOLVE_EXPORT sca_wei_sup
!
! 5.  LOOP PROGRAM STRUCTURE
!
!  LOOP: MAIN LOOP
!
!     Begin master processing loop. It runs over data bases
!
      INCLUDE  'fast.i'
      INTEGER*4 J1, J2, J3, J4, J5, J6, J7, J8, &
     &          NUMSCA_NEW, SNGCHK_CMP, IP1, IP2, IER, IUER
      TYPE ( PLACE__STRU   ) :: PLACE
      TYPE ( B3D__STRU     ) :: B3DOBJ
      TYPE ( B1B3D__STRU   ) :: B1B3DOBJ
      TYPE ( DBOBJ_O__STRU ) :: DBOBJ
      INTEGER*4  R_SOU, RIS_SOU(MO_SOU), &
     &           R_STA, RIS_STA(MO_STA), &
     &           R_BAS, RIS_BAS(MO_BAS)
      LOGICAL*4  WAS_FIRST, LBACK, FL_TPD_DEBUG, FL_TPD_VERB, FL_TPD_READ, FL_TPD_WRITE
      INTEGER*2  ISTAR_LAST, ISTA, IDATYP_SAVE
      INTEGER*4  MJD_UTC_OBS, MJD_TAI_OBS, LUN_DEBUG, LUN_EDIT, LUN_ADDW, &
     &           LUN_SCAW, LUN_IONO, LUN_DTEC, LUN_FREQ
      REAL*8     FJD_1, FRACTC_1, TT, TT_LAST, EPS_SEC, FJDOBS, LJDOBS, &
     &           UTC_OBS, UTC_MINUS_TAI, SOLVE_SNR_MIN, ELEV_DWNT, DWNT_FACTOR, &
     &           CORAMP_CUTOFF, TAU_E_MAX
      PARAMETER  ( DWNT_FACTOR = 1000.0D0 ) ! How the formal uncertainty should be scaled for downweighting
      PARAMETER  ( EPS_SEC = 0.1 ) ! max acceptable diff. in order of observ.
      LOGICAL*4  F__NEXT_COMSEG, DATYP_INQ, FL_USED, FL_USED_GXS, FL_USED_GX, &
     &           FL_USED_GS, FL_GOOD, FL_RECO, LEX, FL_USED_AS_XS, FL_USED_AS_X, &
     &           FL_USED_FUSED, FL_USED_GRP, NORMEQ_EXT 
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 )
      CHARACTER  SOLVE_DIR_DEBUG*128, SOLVE_DEBUG*128, SOLVE_EXPORT*128, &
     &           SOLVE_EXPORT_DIR*128, C_STA(MAX_ARC_STA)*8, FINAM_DEBUG*128, &
     &           FINAM_IONO*128, SOLVE_SNR_MIN_STR*20, WORD*16, BAS*19, &
     &           STR_DTEC_SBA_ERR_SCL*32, REG*3 
      INTEGER*4  LIND, IND(2,MIND), TPD_USE_FLAG
      INTEGER*8  MEM_RSS
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: BAD_OBS, IS_R8_NAN, IS_R8_INF, META_SUPR_INQ, &
     &                       SUPR_INQ
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_UNIT, LTM_DIF
      INTEGER*8, EXTERNAL :: GET_MEMRS
      REAL*8,    EXTERNAL :: RGAUSS
!
      CALL GETENVAR ( 'NORMEQ_EXT', STR )
      IF  ( STR == 'YES' .OR. STR == 'yes' ) THEN
            NORMEQ_EXT = .TRUE.
         ELSE
            NORMEQ_EXT = .FALSE.
      END IF 
!
      CALL GETENVAR ( 'DEBUG_SBA', STR )
      IF  ( STR == 'YES' .OR. STR == 'yes' ) THEN
            FL_DEBUG_SBA = .TRUE.
         ELSE
            FL_DEBUG_SBA = .FALSE.
      END IF 
!
      CALL GETENVAR ( 'PSOLVE_CORAMP_CUTOFF', STR )
      IF  ( ILEN(STR) .GT. 0 ) THEN
            READ ( UNIT=STR, FMT='(F10.5)' ) CORAMP_CUTOFF
         ELSE
            CORAMP_CUTOFF = 1.0D6
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
      TAU_E_MAX = 1.0D0
      CALL GETENVAR ( 'SOLVE_TAU_E_MAX', STR ) 
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F10.5)' ) TAU_E_MAX
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
      FL_WRONG_FREQ_PRINT = .TRUE.
      CALL GETENVAR ( 'SOLVE_TIME_UPWEIGHT', STR )
!
!     str = 'USE_0_TO_3_HR'
!      
      IF ( ILEN(STR) > 0 ) THEN
           CALL EXWORD ( STR, MIND, LIND, IND, '_', IER )
           IF ( LIND .LT. 5 ) THEN
                CALL ERR_LOG ( 8311, IUER, 'LOOP', 'Malformed variable '// &
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
           WRITE (  6, * ) 'LOOP  SOLVE_TIME_UPWEIGHT is set up: TIM_UPWEI_BEG/TIM_UPWEI_END= ', SNGL(TIM_UPWEI_BEG), SNGL(TIM_UPWEI_END)
           WRITE ( 23, * ) 'LOOP  SOLVE_TIME_UPWEIGHT is set up: TIM_UPWEI_BEG/TIM_UPWEI_END= ', SNGL(TIM_UPWEI_BEG), SNGL(TIM_UPWEI_END)
         ELSE
           TIM_UPWEI_BEG = -1.D8
           TIM_UPWEI_END =  1.D8
      END IF
!
      FREQ_K = 23.5D9
!
! --- Setting flag of printing the output intformation at the screen using
! --- "_MN" interface (curses)
!
      K_MN = KSCREEN                      .AND. KBIT ( PRE_IP ( 2 ), INT2(6))  ! Interactide mode
      IF ( KBIT ( PRE_IP ( 3 ), INT2(12)) .AND. REWAY_VERBOSE )  &
     &     K_MN = .FALSE.  ! But supress printout
!                          !  in silent REWAY mode
!
      EVINT4 = EVINT
      EVSTART4 = EVSTART
      IS = 481422
!
! --- Check, whether we need to open the spool file
!
      INQUIRE ( UNIT=23, OPENED=LEX )
      IF ( .NOT. LEX ) THEN
           CALL USE_SPOOL ( 'OS' )
      END IF
      CALL CLRCH    ( SOLVE_SNR_MIN_STR )
      CALL GETENVAR ( 'SOLVE_SNR_MIN', SOLVE_SNR_MIN_STR )
      IF ( ILEN(SOLVE_SNR_MIN_STR) > 0 ) THEN
           READ ( UNIT=SOLVE_SNR_MIN_STR, FMT='(F20.0)', IOSTAT=IER ) SOLVE_SNR_MIN
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8312, IUER, 'LOOP', 'Wrong format of '// &
     &              'environment variable SOLVE_SNR_MIN: '//SOLVE_SNR_MIN_STR )
                RETURN
           END IF
        ELSE
           SOLVE_SNR_MIN = 0.0D0
      END IF
      IF ( ILEN(TEC_NOISE_FILE) > 0 ) THEN
           CALL ERR_PASS  ( IUER, IER )
           CALL PARSE_TCN ( TEC_NOISE_FILE, M__TCN, TCN, L_TCN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8313, IUER, 'LOOP', 'Error in an attempt '// &
     &              'to parse GPS TEC noise file '//TEC_NOISE_FILE )
                RETURN
           END IF
         ELSE
           L_TCN = 0
      END IF
!
! --- This trick is done in order to overcome a bug in HP Fortran90 2.5.1
! --- which erorneosuly puts MOD(I,0) out of cycles and dies due to
! --- IEEE dividing by zero
!
      IF ( EVINT4 .LE. 0 ) EVINT4 = MAX_OBS + 1
!
! --- Open OBSFIL file
!
      FL_11 = KBIT( PRE_IBATCH, INT2(11) )
      IF ( DBNAME_CH(1:1) .NE. '$' ) CALL SBIT ( PRE_IBATCH, INT2(11), 0 )
!
      CALL ACS_OBSFIL ( 'O' )
      IF ( DBNAME_CH(1:1) .NE. '$'  .AND.  FL_11 ) CALL SBIT ( PRE_IBATCH, INT2(11), 1 )
!
! --- Compute average atmosphere parameters
!
      CALL GET_AVG_ATM ( CDBNAM, AVG_ATM )
      NELEM = INT8(NPARAM)*INT8(NPARAM+1)/2
      JA = 1+3*M_GPA
      JB = 1+2*M_GPA
      IDBGN = 1
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
      DO IDB = 1, NDB ! Master loop over data bases
!
! ------ If this data base not selected, kick out
!
         IF(.NOT.KBIT(IDBSEL,IDB)) GO TO 1009 ! skip the database
         IDEND = IDBEND(IDB)
         CALL OBSTM ( FJDOBS, LJDOBS )
!
! ------ Learn NUMDB  -- the number of databases treated by LOOP now
! ------       LDBNAM -- the data base name
! ------       IDBV   -- data base version (in 1-st element of array)
! ------       IDBE   -- number of observations (in 1-st element of array)
!
         CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
         CALL GETENVAR ( 'SOLVE_DEBUG',       SOLVE_DEBUG )
         CALL GETENVAR ( 'SOLVE_DIR_DEBUG',   SOLVE_DIR_DEBUG )
         CALL GETENVAR ( 'SOLVE_EXPORT',      SOLVE_EXPORT )
         CALL GETENVAR ( 'SOLVE_EXPORT_DIR',  SOLVE_EXPORT_DIR )
         IF ( ILEN(SOLVE_EXPORT_DIR) == 0 ) THEN
              SOLVE_EXPORT_DIR = '/tmp'
         END IF
         NOBS = IDEND - IDBGN + 1
         IF ( INDEX ( SOLVE_EXPORT, 'edit' ) > 0 ) THEN
!
! ----------- Open output edit file and write the header
!
              FILOUT_EDIT = TRIM(SOLVE_EXPORT_DIR)//'/'//TRIM(DBNAME_CH)//'_edit.txt'
              LUN_EDIT = GET_UNIT()
              OPEN ( FILE=FILOUT_EDIT, UNIT=LUN_EDIT, STATUS='UNKNOWN', IOSTAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8314, IUER, 'LOOP', 'Error in attempt to '// &
     &                 'open the output editing file '//FILOUT_EDIT )
                   CALL EXIT ( 1 )
              END IF
!
              IL = MIN( 10, ILEN(DBNAME_CH) )
              WRITE ( UNIT=LUN_EDIT, FMT='(A)', IOSTAT=IER ) LABEL__EDIT
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8315, IUER, 'LOOP', 'Error in attempt to '// &
     &                 'write in the output editing file '//FILOUT_EDIT )
                   CALL EXIT ( 1 )
              END IF
              WRITE ( UNIT=LUN_EDIT, FMT='(A)' ) '# '
              WRITE ( UNIT=LUN_EDIT, FMT=152   ) DBNAME_CH, IDBV(1), NOBS, EXP_CODE
 152          FORMAT ( '# Experiment: ', A10, ' Version ', I3, ' Nobs: ', I6, ' Exp_code: ', A )
              WRITE ( UNIT=LUN_EDIT, FMT='(A)' ) '# '
              WRITE ( UNIT=LUN_EDIT, FMT='(A)' ) '# Generated by pSolve on '//GET_CDATE()
              WRITE ( UNIT=LUN_EDIT, FMT='(A)' ) '# '
         END IF
!
         IF ( INDEX ( SOLVE_EXPORT, 'addw' ) > 0 ) THEN
!
! ----------- Open output additive weight file and write the header
!
              FILOUT_ADDW = TRIM(SOLVE_EXPORT_DIR)//'/'//TRIM(DBNAME_CH)//'_addw.txt'
              LUN_ADDW = GET_UNIT()
              IL = MIN( 10, ILEN(DBNAME_CH) )
              OPEN ( FILE=FILOUT_ADDW, UNIT=LUN_ADDW, STATUS='UNKNOWN', IOSTAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8316, IUER, 'LOOP', 'Error in attempt to '// &
     &                 'open the output additive weight file '//FILOUT_ADDW )
                   CALL EXIT ( 1 )
              END IF
              WRITE ( UNIT=LUN_ADDW, FMT='(A)', IOSTAT=IER ) LABEL__ADDW
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8317, IUER, 'LOOP', 'Error in attempt to '// &
     &                 'write in the output editing file '//FILOUT_ADDW )
                   CALL EXIT ( 1 )
              END IF
!
              WRITE ( UNIT=LUN_ADDW, FMT='(A)' ) '# '
              WRITE ( UNIT=LUN_ADDW, FMT=152   ) DBNAME_CH, IDBV(1), NOBS, EXP_CODE
              WRITE ( UNIT=LUN_ADDW, FMT='(A)' ) '# '
              WRITE ( UNIT=LUN_ADDW, FMT='(A)' ) '# Generated by pSolve on '//GET_CDATE()
              WRITE ( UNIT=LUN_ADDW, FMT='(A)' ) '# '
         END IF
!
         IF ( INDEX ( SOLVE_EXPORT, 'sca_wei' ) > 0 ) THEN
              FILOUT_SCAW= TRIM(SOLVE_EXPORT_DIR)//'/'//TRIM(DBNAME_CH)//'_scaw.txt'
              LUN_SCAW = GET_UNIT()
              IL = MIN( 10, ILEN(DBNAME_CH) )
              OPEN ( FILE=FILOUT_SCAW, UNIT=LUN_SCAW, STATUS='UNKNOWN', IOSTAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8318, IUER, 'LOOP', 'Error in attempt to '// &
     &                 'open the output additive weight file '//FILOUT_SCAW )
                   CALL EXIT ( 1 )
              END IF
              WRITE ( UNIT=LUN_SCAW, FMT='(A)', IOSTAT=IER ) LABEL__SCAW
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8319, IUER, 'LOOP', 'Error in attempt to '// &
     &                 'write in the output editing file '//FILOUT_SCAW )
                   CALL EXIT ( 1 )
              END IF
!
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# '
              WRITE ( UNIT=LUN_SCAW, FMT=152   ) DBNAME_CH, IDBV(1), NOBS, EXP_CODE
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# '
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# Generated by pSolve on '//GET_CDATE()
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# '
!
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# Obs_ind    6:11   I6    Observation index'
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# Time       18:38  A21   Observation time in TAI'
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# Scan_name  45:60  A16   Scan name'
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# Station1   67:74  A8    The first  station of a baseline'
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# Station2   76:83  A8    The second station of a baseline'
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# Source     90:97  A8    Source name'
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# Sol_typ   104:104 A1    Solution type: x -- X-band, x -- S-band only, d -- dual-band, g -- group delay only, f -- fused'
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# Err       108:120 D13.6 Delay error with applying reweighting. Units: sec'
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# Sup_x     134:134 A1    Suppression status of X-band only observations: T -- use, F -- not to use, N -- not available'
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# Sup_s     136:136 A1    Suppression status of S-band only observations: T -- use, F -- not to use, N -- not available'
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# Sup_d     138:138 A1    Suppression status of dual-band   observations: T -- use, F -- not to use, N -- not available'
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# Sup_f     140:140 A1    Suppression status of fused       observations: T -- use, F -- not to use, N -- not available'
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# Sup_g     142:142 A1    Suppression status of fused       observations: T -- use, F -- not to use, N -- not available'
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# Der_ra    154:164 D11.4 Partial derivative of path delay over right ascension. Units: sec'
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '# Der_del   166:176 D11.4 Partial derivative of path delay over declination.     Units: sec'
              WRITE ( UNIT=LUN_SCAW, FMT='(A)' ) '#'
         END IF
!
         IF ( GIM_COLLECT_INFO          .OR.   &
     &        SOLVE_DEBUG(1:1)  == '3'  .OR.   &
     &        SOLVE_DEBUG(1:1)  == '4'  .OR.   &
     &        SOLVE_DEBUG(1:1)  == '5'  .OR.   &
     &        SOLVE_DEBUG(1:1)  == '6'  .OR.   &
     &        SOLVE_DEBUG(1:1)  == '7'  .OR.   &
     &        SOLVE_DEBUG(1:1)  == '8'  .OR.   &
     &        SOLVE_DEBUG(1:1)  == '9'  .OR.   &
     &        SOLVE_DEBUG(1:8)  == 'PUT_IONO'  .OR. &
     &        SOLVE_DEBUG(1:8)  == 'OUT_IONO'  .OR. &
     &        SOLVE_DEBUG(1:8)  == 'OUT_ELEV'  .OR. &
     &        SOLVE_DEBUG(1:8)  == 'DTEC_OUT'     ) THEN
!
              IF ( ILEN(SOLVE_DIR_DEBUG) == 0 ) SOLVE_DIR_DEBUG = '/tmp'
!
              LUN_DEBUG = GET_UNIT()
              IF ( CDBNAM(IDB)(1:1) == '$' ) THEN
                   FINAM_DEBUG = SOLVE_DIR_DEBUG(1:I_LEN(SOLVE_DIR_DEBUG))//'/'// &
     &                           CDBNAM(IDB)(2:ILEN(CDBNAM(IDB)))//'.del'
                 ELSE
                   FINAM_DEBUG = SOLVE_DIR_DEBUG(1:I_LEN(SOLVE_DIR_DEBUG))//'/'// &
     &                           CDBNAM(IDB)(1:ILEN(CDBNAM(IDB)))//'.del'
              END IF
              IF ( SOLVE_DEBUG(1:8) == 'PUT_IONO' ) THEN
                   FINAM_DEBUG = SOLVE_DIR_DEBUG(1:I_LEN(SOLVE_DIR_DEBUG))//'/'// &
     &                           TRIM(DBNAME_CH)//'_iono.info'
                   OPEN ( UNIT=LUN_DEBUG, FILE=FINAM_DEBUG, STATUS='UNKNOWN', &
     &                    IOSTAT=IER )
              END IF
              IF ( SOLVE_DEBUG(1:8) == 'GET_IONO' ) THEN
                   FINAM_DEBUG = SOLVE_DIR_DEBUG(1:I_LEN(SOLVE_DIR_DEBUG))//'/'// &
     &                           'iono.dat'
                   OPEN ( UNIT=LUN_DEBUG, FILE=FINAM_DEBUG, STATUS='UNKNOWN', &
     &                    ACCESS='APPEND', IOSTAT=IER )
              END IF
              IF ( SOLVE_DEBUG(1:8) == 'OUT_IONO' ) THEN
                   FINAM_DEBUG = SOLVE_DIR_DEBUG(1:I_LEN(SOLVE_DIR_DEBUG))//'/'// &
     &                           'iono.txt'
                   OPEN ( UNIT=LUN_DEBUG, FILE=FINAM_DEBUG, STATUS='UNKNOWN', &
     &                    ACCESS='APPEND', IOSTAT=IER )
              END IF
              IF ( SOLVE_DEBUG(1:8) == 'OUT_ELEV' ) THEN
                   FINAM_DEBUG = SOLVE_DIR_DEBUG(1:I_LEN(SOLVE_DIR_DEBUG))//'/'// &
     &                           TRIM(DBNAME_CH)//'_elev.info'
                   OPEN ( UNIT=LUN_DEBUG, FILE=FINAM_DEBUG, STATUS='UNKNOWN', &
     &                    IOSTAT=IER )
              END IF
              IF ( SOLVE_DEBUG(1:8) == 'DTEC_OUT' ) THEN
                   FINAM_DEBUG = SOLVE_DIR_DEBUG(1:I_LEN(SOLVE_DIR_DEBUG))//'/'// &
     &                           TRIM(DBNAME_CH)//'_dtec.out'
                   OPEN ( UNIT=LUN_DEBUG, FILE=FINAM_DEBUG, STATUS='UNKNOWN', &
     &                    IOSTAT=IER )
              END IF
!
              IF ( SOLVE_DEBUG(1:2) == '48' ) THEN
                   FINAM_DEBUG = SOLVE_DIR_DEBUG(1:I_LEN(SOLVE_DIR_DEBUG))//'/'// &
     &                           TRIM(DBNAME_CH)//'_dtec.info'
                   OPEN ( UNIT=LUN_DEBUG, FILE=FINAM_DEBUG, STATUS='UNKNOWN', &
     &                    IOSTAT=IER )
              END IF
!
              IF ( SOLVE_DEBUG(1:2) == '66' ) THEN
                   FINAM_DEBUG = SOLVE_DIR_DEBUG(1:I_LEN(SOLVE_DIR_DEBUG))//'/'// &
     &                           TRIM(DBNAME_CH)//'_stru.info'
                   OPEN ( UNIT=LUN_DEBUG, FILE=FINAM_DEBUG, STATUS='UNKNOWN', &
     &                    IOSTAT=IER )
              END IF
!
              IF ( GIM_COLLECT_INFO ) THEN
                   FINAM_DEBUG = TRIM(GIM_INFO_DIR)//'/'//TRIM(DBNAME_CH)//'_iono.txt'
                   OPEN ( UNIT=LUN_DEBUG, FILE=FINAM_DEBUG, STATUS='UNKNOWN', &
     &                    IOSTAT=IER )
              END IF
!
              OPEN ( UNIT=LUN_DEBUG, FILE=FINAM_DEBUG, STATUS='UNKNOWN', &
     &               IOSTAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH  ( STR )
                   CALL INCH   ( IER, STR )
                   CALL ERR_LOG ( 8320, IUER, 'LOOP', 'Environment variable '// &
     &                 'SOLVE_DEBUG is set. Failure to open '// &
     &                 'output debuggin file '// &
     &                  FINAM_DEBUG(1:I_LEN(FINAM_DEBUG))//' -- error '//STR )
                   RETURN
              END IF
!
              IF ( GIM_COLLECT_INFO ) THEN
                   WRITE ( LUN_DEBUG, '(A)' ) LABEL__IONO
                ELSE
                   WRITE ( LUN_DEBUG, '(A)' ) '# Solve Delay Output. Format version of 2023.12.10'
              END IF
              IF ( SOLVE_DEBUG(1:8) == 'PUT_IONO' ) THEN
                   WRITE ( LUN_DEBUG, '(A)' ) '#  '
                   WRITE ( LUN_DEBUG, '(A)' ) '# PUT_IONO mode'
              END IF
              IF ( SOLVE_DEBUG(1:8) == 'OUT_ELEV' ) THEN
                   WRITE ( LUN_DEBUG, '(A)' ) '#  '
                   WRITE ( LUN_DEBUG, '(A)' ) '# OUT_ELEV mode'
              END IF
              WRITE ( LUN_DEBUG, '(A)' ) '#  '
              WRITE ( LUN_DEBUG, '(A,I3,A,I6,A)' ) '# Experiment: '//CDBNAM(IDB)//' Version ', IDBV(IDB), &
     &                                             '  Nobs: ', IDEND - IDBGN + 1, ' Exp_code: '//EXP_CODE
              WRITE ( LUN_DEBUG, '(A)' ) '#  '
              WRITE ( LUN_DEBUG, '(A)' ) '# Generated on '//GET_CDATE()
              WRITE ( LUN_DEBUG, '(A)' ) '#  '
              WRITE ( LUN_DEBUG, '(A)' ) '# File format: '
              WRITE ( LUN_DEBUG, '(A)' ) '# '
              IF ( SOLVE_DEBUG(1:1) == '3'  ) THEN
                   WRITE ( LUN_DEBUG, '(A)' ) '#   1:5    I5      Observation index'
                   WRITE ( LUN_DEBUG, '(A)' ) '#   7:16   A10     Scan name'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  24:46   A23     Fringe reference time tag in pseudo-UTC'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  48:55   A8      First (reference) IVS station name of the baseline'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  50:50   A1      Baseline name delimiter /'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  57:64   A8      Second (remote) IVS station name of the baseline'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  66:73   A8      IVS source name'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  76:76   A1      Quality code of the upper (high frequency) band'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  79:79   A1      Quality code of the lower (low  frequency) band'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  82:90   F9.6    Fringe amplitude X-band'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  93:101  F9.6    Fringe amplitude S-band'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 104:110  F7.2    SNR at X-band'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 112:118  F7.2    SNR at X-band'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 121:121  L1      Flag: used in solution (T) or not (F)'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 124:129  I6      Observation index in PIMA'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 132:163  B32     AUTO_SUP'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 165:196  B32     USER_SUP'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 198:229  B32     USER_REC'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 232:245  D14.7   Ionospheric frequency Hz, low band'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 247:260  D14.7   Ionospheric frequency Hz, upper band'
                   WRITE ( LUN_DEBUG, '(A)' ) '#'
                   WRITE ( LUN_DEBUG, '(A)' ) '#Nobs Scan_name        UTC time tag            Baseline          Source   Qx Qs   Ampl_X     Ampl_S     SNR_X   SNR_S   Flag  Ind  AUTO_SUP                         USER_SUP                         USER_REC                           Frq_iono_low   Frq_iono_high'
                   WRITE ( LUN_DEBUG, '(A)' ) '#'
                ELSE IF ( SOLVE_DEBUG(1:2) == '44' ) THEN
                   WRITE ( LUN_DEBUG, '(A)' ) '#   1:5   I5   -- Index of the observation'
                   WRITE ( LUN_DEBUG, '(A)' ) '#   7:29  A23  -- TAI date'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  31:38  A8   -- IVS source name   '
                   WRITE ( LUN_DEBUG, '(A)' ) '#  40:47  A8   -- IVS station #1'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  51:58  A8   -- IVS station #2'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  61:65  F5.2 -- Station #1 elevation above the horizon (degrees)'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  67:71  F5.2 -- Station #2 elevation above the horizon (degrees)'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  75:80  F6.2 -- Station #1 azimuth (degrees)'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  83:88  F6.2 -- Station #2 azimuth (degrees)'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  91:98  F8.3 -- Differential TEC: TEC(station 2) - TEC(station 1) in TECU'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  91:98  F8.3 -- Differential TEC: TEC(station 2) - TEC(station 1) in TECU'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 100:107 F8.3 -- Standerd deviation of differential TEC in TECU'
                   WRITE ( LUN_DEBUG, '(A)' ) '#'
                   WRITE ( LUN_DEBUG, '(A)' ) '# Ind Date Tai                Sou name Sta1       Sta2      El1   El2     Az1     Az2      dTEC    dTec err'
                   WRITE ( LUN_DEBUG, '(A)' ) '#'
                ELSE IF ( SOLVE_DEBUG(1:2) == '46' ) THEN
                   WRITE ( LUN_DEBUG, '(A)' ) '#   1:5   I5    -- Index of the observation'
                   WRITE ( LUN_DEBUG, '(A)' ) '#   7:29  A23   -- TAI date'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  31:38  A8    -- station #1'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  40:47  A8    -- station #2'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  49:56  A8    -- source name'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  59:59  L1    -- Flag: T observations is used in the solution'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  62:76  D15.7 -- U-coordinate: projection of the baseline vcector to the plane to tangentional source in wavelnghts'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  78:92  D15.7 -- V-coordinate: projection of the baseline vcector to the plane to tangentional source in wavelnghts'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  95:109 D15.7 -- Partial derivative on source position right ascension. Units: s'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 111:125 D15.7 -- Partial derivative on source position decliniation. Units: s '
                   WRITE ( LUN_DEBUG, '(A)' ) '# 128:142 D15.7 -- Partial derivative on atmosphere path delay in zenith direction of station 1'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 144:158 D15.7 -- Partial derivative on atmosphere path delay in zenith direction of station 2      Obs error'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 161:175 D15.7 -- Observation weight. Units: 1/s'
                   WRITE ( LUN_DEBUG, '(A)' ) '#'
                   WRITE ( LUN_DEBUG, '(A)' ) '# Ind Date Tai                Sta1     Sta2     Sou name  F   u-coord         v-coord          d tau/d alpha   d tau/d delta      d tau/atm_1     d tau/atm_2    recipr. weight'
                   WRITE ( LUN_DEBUG, '(A)' ) '#'
                ELSE IF ( SOLVE_DEBUG(1:1) == '7'  .OR.  &
     &                    SOLVE_DEBUG(1:1) == '8'        ) THEN
!
                   WRITE ( LUN_DEBUG, '(A)' ) '#   1:5    I5      Observation index'
                   WRITE ( LUN_DEBUG, '(A)' ) '#   7:16   A10     Scan name'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  18:40   A23     Fringe reference time tag in pseudo-UTC'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  42:49   A8      First (reference) IVS station name of the baseline'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  50:50   A1      Baseline name delimiter /'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  51:58   A8      Second (remote) IVS station name of the baseline'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  60:67   A8      IVS source name'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  70:70   A1      Quality code of the upper (high frequency) band'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  71:71   A1      Delimiter /'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  73:73   A1      Quality code of the lower (low  frequency) band'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  75:95   D21.14  Group delay at the upper band with ambiguity resolved'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  97:117  D21.14  Group delay at the lower band with ambiguity resolved'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 119:139  D21.14  Phase delay rate at the upper band'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 141:161  D21.14  Phase delay rate at the lower band'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 163:170  F8.5    Fringe phase at the upper band'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 172:179  F8.5    Fringe phase at the lower band'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 181:188  F8.3    SNR at the upper band'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 190:197  F8.3    SNR at the lower band'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 199:208  D10.3   Formal uncertainty of group delay at the upper band reported by fringe search software'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 210:219  D10.3   Formal uncertainty of group delay at the lower band reported by fringe search software'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 221:221  L1      Flag, whether the observation is used in the solution: T -- used, F -- not used'
                   WRITE ( LUN_DEBUG, '(A)' ) '# '
                ELSE IF ( SOLVE_DEBUG(1:1) == '9' ) THEN
                   WRITE ( LUN_DEBUG, '(A)' ) '#   1:5    I5      Observation index'
                   WRITE ( LUN_DEBUG, '(A)' ) '#   7:16   A10     Scan name'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  18:40   A23     Fringe reference time tag in pseudo-UTC'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  42:49   A8      First (reference) IVS station name of the baseline'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  50:50   A1      Baseline name delimiter /'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  51:58   A8      Second (remote) IVS station name of the baseline'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  60:67   A8      IVS source name'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  70:70   A1      Quality code of the upper (high frequency) band'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  71:71   A1      Delimiter /'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  73:73   A1      Quality code of the lower (low  frequency) band'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  75:95   D19.12  O-C in delay'
                   WRITE ( LUN_DEBUG, '(A)' ) '#  97:117  D11.4   Reciprocal weight for delay'
                   WRITE ( LUN_DEBUG, '(A)' ) '# 221:221  L1      Flag, whether the observation is used in the solution: T -- used, F -- not used'
                   WRITE ( LUN_DEBUG, '(A)' ) '# '
                ELSE IF ( SOLVE_DEBUG(1:8) == 'OUT_ELEV' ) THEN
                   WRITE ( LUN_DEBUG, '(A)' ) '# NOBS   7:12  I6    Obseration index'
                   WRITE ( LUN_DEBUG, '(A)' ) '# TIM   19:26  F8.1  Time since the session nominal start'
                   WRITE ( LUN_DEBUG, '(A)' ) '# STA1  33:48  A8    First  station of a baseline'
                   WRITE ( LUN_DEBUG, '(A)' ) '# STA2  42:49  A8    Second station of a baseline'
                   WRITE ( LUN_DEBUG, '(A)' ) '# EL1   57:64  F8.5  Elevation of first  station in radians'
                   WRITE ( LUN_DEBUG, '(A)' ) '# EL2   67:73  F8.5  Elevation of second station in radians'
                   WRITE ( LUN_DEBUG, '(A)' ) '# AZ1   79:86  F8.5  Azimith   of first  station in radians'
                   WRITE ( LUN_DEBUG, '(A)' ) '# AZ2   88:95  F8.5  Azimith   of second station in radians'
                   WRITE ( LUN_DEBUG, '(A)' ) '# '
              END IF
              FL_TPD_READ = .FALSE.
         END IF
!
         IF ( IONOV_USE == IONOV__GEN ) THEN
              LUN_IONO = GET_UNIT()
              FINAM_IONO = TRIM(IONOV_DIR)//'/'//TRIM(DBNAME_CH)//'.ivm'
              INQUIRE ( FILE=FINAM_IONO, EXIST=FILE_IONO_EX ) 
              IF ( FILE_IONO_EX ) THEN
                   IF ( G_WARNING ) THEN
                        WRITE ( 6, '(A)' ) 'External ionosphere path delay file '// &
     &                                      TRIM(FINAM_IONO)//' already exists. Do not overwrite it.'
                   END IF
                ELSE IF ( .NOT. FILE_IONO_EX ) THEN
                   OPEN ( UNIT=LUN_IONO, FILE=FINAM_IONO, STATUS='UNKNOWN', &
     &                    IOSTAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH  ( STR )
                        CALL INCH   ( IER, STR )
                        CALL ERR_LOG ( 8321, IUER, 'LOOP', 'Failure to open '// &
     &                      'output ionosphre file '//FINAM_IONO )
                        RETURN
                   END IF
                   WRITE ( UNIT=LUN_IONO, FMT='(A)' ) '# External iono path delay. Format version of 2016.12.29'
                   WRITE ( UNIT=LUN_IONO, FMT='(A)' ) '#'
                   WRITE ( UNIT=LUN_IONO, FMT='(A,I6)' ) '# Experiment: '//DBNAME_CH(1:10)// &
     &                                                   '  Nobs: ', IDEND - IDBGN + 1
                   WRITE ( UNIT=LUN_IONO, FMT='(A)' ) '# '
                   WRITE ( UNIT=LUN_IONO, FMT='(A)' ) '# Generated by Solve'
                   WRITE ( UNIT=LUN_IONO, FMT='(A)' ) '# Generated on '//GET_CDATE()
                   WRITE ( UNIT=LUN_IONO, FMT='(A)' ) '# '
              END IF
         END IF
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
! ------ Fill internal fields of B3DOBJ
!
! ------ Transfer database name to B3DOBJ.DBNAME
!
         B3DOBJ%DBNAME = CDBNAM(IDB)
         B3DOBJ%DBVER = INT4 ( IDBV(IDB) )
!
! ------ Forming field B3DOBJ.DBNAME_MES
!
         CALL CLRCH ( B3DOBJ%DBNAME_MES )
         B3DOBJ%DBNAME_MES = B3DOBJ%DBNAME
         B3DOBJ%DBNAME_MES(12:) = '<'
         CALL INCH ( B3DOBJ%DBVER, B3DOBJ%DBNAME_MES(13:) )
         B3DOBJ%DBNAME_MES( I_LEN(B3DOBJ%DBNAME_MES)+1: ) = '>'
         B3DOBJ%DBNAME_LEN = I_LEN ( B3DOBJ%DBNAME_MES )
!
! ------ Initialization
!
         B3DOBJ%JD_NOM_FIRST = 0.0D0
         B3DOBJ%JD_NOM_LAST  = 0.0D0
         B3DOBJ%JD_ACT_FIRST = 0.0D0
         B3DOBJ%JD_ACT_LAST  = 0.0D0
!
! ------ Read station names, status array, eccentricity data, monument
! ------ names, and set up a correspondence table between the stations
! ------ in NAMFIL (JSIT's) and those in PARFIL (ISIT's).
!
         CALL USE_GLBFIL_4 ( 'OR' )
         CALL NCORT ( JSITN, JSITI, JCAPPL, NUMSTA, ITT, IDB, &
     &                IDATYP, ITTB, ET, SE, SS, OBCAPL, MCAPL, JCAVAL, &
     &                LATS, HEIGHTS, AX_TYPES, AX_OFFS, BARO_CALS, &
     &                BARO_HEIGHTS, JCAFFL, FCAL_NAMES, NFCAL, NAMSTA, &
     &                CALCV )
         CALL USE_GLBFIL_4 ( 'WC' )
!
! ------ Send the user some data base status information
!
         IF ( K_MN ) THEN
              call setcr_mn ( 0, 1 )
              call addstr_f ( "Making normal equations for database  "// &
     &                         b3dobj%dbname_mes )
              call setcr_mn ( 78, 1 )
              call refresh_mn()
         ENDIF
         IF ( FAST_DBG .EQ. F__APP ) THEN
              WRITE ( 6, * ) ' PROC/LOOP started for '//B3DOBJ%DBNAME
         END IF
!
! ------ Now do inner loop running over the observations
!
         POLYONLY = .TRUE. ! ???
         DO I=1,NUMSTA
            DO J=1,NUMCLK(I)
               IF ( KBIT( LCLK(J), int2(13)).AND.KBIT(ICLSTA(1,J),I)) THEN
                    POLYONLY=.false.
               ENDIF
            ENDDO
         ENDDO
!
        IF ( FAST_DBG .EQ. F__TIM ) THEN
             CALL TIM_GET ( 'PROC-01' )
             CALL TIM_INIT()
        END IF
!
! ----- Reading NAMFILE ASM cards -- information about a priori clock model
!
        ICONT_I2 = 1
        DO 510 J1=1,M_ACM
           CALL CLRCH   ( JBUF )
           CALL GETCARD ( IDB, 'ACM ', ICONT_I2, JBUF, IERR_I2 )
           ICONT_I2 = 0
           IF ( IERR_I2 .EQ. -3 ) THEN
!
! ------------- There were no such a card in NAMFIL.
!
                STAT_ACM(J1)   = '        '
                CLOOF_ACM(J1)  = 0.0
                CLODR_ACM(J1)  = 0.0
                L_ACM = 0
                ICONT_I2 = 1
              ELSE IF  ( IERR_I2 .NE. 0 ) THEN
!
! ------------- Error in reading NAMFIL
!
                WRITE ( 6, * ) ' ierr_i2 = ',ierr_i2,' j1=',j1
                CALL FERR ( INT2(2823), 'CRES(first) Error in reading of '// &
     &              'ACM card', INT2(0), INT2(0) )
                STOP 'CRES -- Abnormnal termination'
              ELSE
!
! ------------- Decoding a card
!
                IF ( JBUF(15:15) == CHAR(0) ) THEN
!
! ------------------ Fix corrupted card
!
                     CALL CLRCH ( JBUF(15:22) )
                END IF 
                READ ( JBUF, &
     &                '(5X,I2,1X,I2,1X,I2,1X,A8,1X,D23.15,1X,D23.15)' ) &
     &               IP1, L_ACM, IP2, STAT_ACM(J1), CLOOF_ACM(J1), CLODR_ACM(J1)
                IF ( ILEN(STAT_ACM(J1)) .EQ. 0 ) THEN
!
! ------------------ Card is empty
!
                     CLOOF_ACM(J1)  = 0.0
                     CLODR_ACM(J1)  = 0.0
                END IF
!
! ------------- Fix crazy numbers
!
                IF ( DABS(CLOOF_ACM(J1)) < 1.D-16 ) THEN
                     CLOOF_ACM(J1) = 0.0D0
                END IF
                IF ( DABS(CLOOF_ACM(J1)) > 1.D5 ) THEN
                     CLOOF_ACM(J1) = 0.0D0
                END IF
                IF ( DABS(CLODR_ACM(J1)) < 1.D-20 ) THEN
                     CLODR_ACM(J1) = 0.0D0
                END IF
                IF ( DABS(CLODR_ACM(J1)) > 1.D5 ) THEN
                     CLODR_ACM(J1) = 0.0D0
                END IF
           END IF
 510    CONTINUE 
!
        LBACK = .FALSE. ! Flag indicating that it is the first run through this
!                       ! database
        B3DOBJ%R_SOU = 0
        B3DOBJ%R_STA = 0
        B3DOBJ%R_BAS = 0
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
        FL_TPD_VERB  = .FALSE.
        CALL GETENVAR ( 'TPD_USE', STR )
        IF ( STR .EQ. 'DEBUG' ) THEN
             FL_TPD_DEBUG = .TRUE.
           ELSE IF ( STR .EQ. 'VERB' ) THEN
             FL_TPD_DEBUG = .TRUE.
             FL_TPD_VERB  = .TRUE.
           ELSE IF ( STR .EQ. 'YES' ) THEN
             FL_TPD_DEBUG = .FALSE.
           ELSE
             TPD_FLAG = TPD__UNDF
        END IF
        TPD_USE_FLAG = TPD_FLAG
!
        FL_TPD_WRITE = .FALSE.
        FL_TPD_READ  = .FALSE.
        IF ( FL_TPD_VERB ) THEN
             WRITE ( 6, * ) 'loop-679 TPD_FLAG = ', TPD_FLAG, ' TPD_USE_FLAG= ', TPD_USE_FLAG, &
     &                      ' FL_TPD_READ= ', FL_TPD_READ, ' FL_TPD_WRITE= ', FL_TPD_WRITE
             WRITE ( 6, '(//A)' ) ' '
             CALL PAUSE ( 'PROC(loop)-679' )
        END IF
        IF ( TPD_USE_FLAG == TPD__USE  .OR.  TPD_USE_FLAG == TPD__UPD ) THEN
!
! ---------- Initialize TPD data structure
!
             CALL ERR_PASS ( IUER, IER )
             CALL TPD_INIT ( TPD, DBNAME_CH, VTD_CONF_USE, IDBEND-IDBGN+1, &
     &                       INT4(NUMSTA), INT4(NUMSTR), IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8322, IUER, 'LOOP', 'Failure during '// &
     &                'attempt to initialize TPD data structure while '// &
     &                'experiment '//B3DOBJ%DBNAME_MES//' was beging '// &
     &                'processing' )
                  RETURN
             END IF
        END IF
        IF ( TPD_USE_FLAG == TPD__USE  ) THEN
!
! ---------- Read TPD file, parse it and store in TPD
!
             CALL ERR_PASS ( IUER, IER )
             CALL TPD_READ ( TPD, TPD_USE_FLAG, VTD_CONF_USE, DBNAME_CH, &
     &                       N_TPD_INIT, TPD_INIT_LIST, FL_TPD_DEBUG, &
     &                       FL_TPD_READ, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8323, IUER, 'LOOP', 'Failure during '// &
     &                'attempt to read TPD data strucuture while '// &
     &                'experiment '//B3DOBJ%DBNAME_MES//' was beging '// &
     &                'processing' )
                  RETURN
             END IF
             IF ( .NOT. FL_TPD_READ ) FL_TPD_WRITE = .TRUE.
           ELSE IF ( TPD_USE_FLAG == TPD__UPD ) THEN
             FL_TPD_WRITE = .TRUE.
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
                  CALL ERR_LOG ( 8324, IUER, 'LOOP', 'Failure in an attempt '// &
     &                'to load external tropospheric path delay file '// &
     &                'for database '//DBNAME_CH )
                  RETURN
             END IF
        END IF
        IF ( FL_TPD_VERB ) THEN
             WRITE ( 6, * ) 'loop-730 TPD_FLAG = ', TPD_FLAG, ' TPD_USE_FLAG= ', TPD_USE_FLAG, &
     &                      ' FL_TPD_READ= ', FL_TPD_READ, ' FL_TPD_WRITE= ', FL_TPD_WRITE
             WRITE ( 6, '(//A)' ) ' '
             CALL PAUSE ( 'PROC(loop)-730' )
        END IF
!
!!    write ( 6, * ) 'LOOP-1009 vtd_conf_use= ', trim(vtd_conf_use), ' fl_tpd_read= ', fl_tpd_read, VTD_ADR ! %%%%
        IF ( ILEN(VTD_CONF_USE) > 0  .AND.  .NOT. FL_TPD_READ ) THEN
             IF ( VTD_ADR == 0 ) THEN
                  SIZEOF_VTD = SIZEOF(VTD_PTR)
                  CALL ERR_PASS ( IUER, IER )
                  CALL GRAB_MEM ( IER, MEM_LEN, MEM_ADR, 1, &
     &                            INT8(SIZEOF_VTD), VTD_ADR )
                  IF ( IER .NE. 0 ) THEN
                       CALL CLRCH ( STR )
                       CALL IINCH ( SIZEOF_VTD, STR )
                       CALL ERR_LOG ( 8325, IUER, 'LOOP', 'Failure to '// &
     &                     'allocate '//STR(1:I_LEN(STR))// &
     &                     ' bytes of dynamic memory for VTD' )
                       RETURN
                  END IF
!
                  VTD_STATUS = VTD__ALLC
                  CALL ERR_PASS ( IUER, IER )
                  CALL VTD_INIT ( %VAL(VTD_ADR), IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 8326, IUER, 'LOOP', 'Failure to '// &
     &                      'initialize vtd object' )
                       RETURN
                  END IF
                  VTD_STATUS = VTD__INIT
                ELSE
                  MEM_LEN = 0
             END IF
!
             CALL ERR_PASS ( IUER, IER )
             CALL PRE_VTD  ( %VAL(VTD_ADR), VTD_STATUS, VTD_CONF_USE, &
     &                       FL_VTD_IONO, VTD_IONO_SCALE, IER )
             IF ( IER .NE. 0 ) THEN
                  IF ( SOLVE_DEBUG .NE. 'GET_IONO' ) THEN
                       CALL ERR_LOG ( 8327, IUER, 'LOOP', 'Failure during '// &
     &                     'an attempt to execute pre_vtd routines '// &
     &                     'while processing  database '//B3DOBJ%DBNAME_MES )
                       RETURN
                     ELSE
                       CALL CLRCH ( SOLVE_DEBUG )
                       CLOSE ( UNIT=LUN_DEBUG )
                  END IF
             END IF
           ELSE 
             VTD_IONO_SCALE = 1.0D0
        END IF
 910    CONTINUE
!
! ----- Initialization of counters:
! ----- OBCOUNTR   -- Counter of observations for decimation
! ----- SKIP_COUNT -- Counter of skipped observations before reading user
! -----               partials. Needed for keeping position in the partials
! -----               files
!
        OBCOUNTR   = 0
        SKIP_COUNT = 0
        B3DOBJ%SUWSQ_TAU = 0.0
        B3DOBJ%SUWSQ_FRE = 0.0
!
! ----- Open user_partial scratch file, if there is one
!
        IF ( KUSER_PART .AND. ( ARC_USER_PART > 0 .OR.  NUM_USER_PART > 0 ) ) THEN
             FNAME_PART = PRE_SCR_DIR(1:PRE_SD_LEN)//'PART'//PRE_LETRS
             CALL BIN_OPEN ( FNAME_PART, FILDES_PART, 'O' )
        ENDIF
!
        IF ( FAST_MODE .EQ. F__B3D  ) THEN
!
! ---------- Calculation map of correspondence between parameters in FULL
! ---------- and in B3D matrices
!
             CALL ERR_PASS ( IUER, IER )
             CALL MAP_PARAM ( FAST_MODE, FAST_DBG, B3DOBJ, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8328, IUER, 'LOOP', 'Error '// &
     &                'during calculation map of correspondence '// &
     &                'between parameters in FULL and in B3D matrices '// &
     &                'while database '//B3DOBJ%DBNAME_MES//' was processing' )
                  RETURN
             END IF
!
! ---------- Grabbing dynamic memory for internal fields of B3DOBJ
!
             B3DOBJ%MEM_STAT   = F__MFR
             B1B3DOBJ%MEM_STAT = F__UND
!
             CALL ERR_PASS    ( IUER, IER )
             CALL B3D_GETMEM  ( FAST_COV, B3DOBJ, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8329, IUER, 'LOOP', 'Error '// &
     &                'during getting dynamic memory for data structure '// &
     &                'used in B3D algorithm while database '//B3DOBJ%DBNAME_MES// &
     &                ' was processing' )
                  RETURN
             END IF
          ELSE IF ( FAST_MODE .EQ. F__B1B3D  ) THEN
!
! ---------- Calculation map of correspondence between parameters in FULL
! ---------- and in B1B3D matrices
!
             CALL ERR_PASS  ( IUER, IER )
             CALL MAP_PARAM ( FAST_MODE, FAST_DBG, B3DOBJ, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8330, IUER, 'LOOP', 'Error '// &
     &                'during calculation of map of correspondence '// &
     &                'between parameters in FULL and in B3D matrices '// &
     &                'while database '//B3DOBJ%DBNAME_MES//' was processing' )
                  RETURN
             END IF
!
! ---------- Grabbing dynamic memory for internal fields of B3DOBJ
!
             CALL ERR_PASS     ( IUER, IER )
             CALL B1B3D_GETMEM ( B3DOBJ, B1B3DOBJ, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8331, IUER, 'LOOP', 'Error '// &
     &                'during getting dynamic memory for data structure '// &
     &                'used in B3D algorithm while database '//B3DOBJ%DBNAME_MES// &
     &                ' was processing' )
                  RETURN
             END IF
           ELSE IF ( FAST_MODE .EQ. F__NONE   .OR. &
     &               FAST_MODE .EQ. F__PRD    .OR. &
     &               FAST_MODE .EQ. F__B1D          ) THEN
             B3DOBJ%N_PAR = NPARAM  ! Total number of parameters
!
! ---------- Printing debug infroamtion in the file /tmp/param.fil when
! ---------- FAST_DBG = F__PRI
!
             IF ( FAST_DBG .EQ. F__PRI ) THEN
                  CALL ERR_PASS  ( IUER, IER )
                  CALL DBG_PARAM ( B3DOBJ, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 8332, IUER, 'LOOP', 'Error '// &
     &                     'in debigging printout while database '// &
     &                      B3DOBJ%DBNAME_MES//' was processing' )
                       RETURN
                  END IF
             END IF
           ELSE
             CALL CLRCH (            STR )
             CALL INCH  ( FAST_MODE, STR )
             CALL ERR_LOG ( 8333, IUER, 'LOOP', 'Internal error: '// &
     &           'unsupported value  FAST_MODE:  FAST_MODE='//STR )
             RETURN
        END IF
        IF ( FAST_DBG .EQ. F__PRI ) THEN
             WRITE ( 6, 210 )  b3dobj%dbname_mes, fast_mode, fast_dbg, fast_cov
 210         format ( 1X,' PROC:   session ',a,' fast_mode=',i4, &
     &                   ' fast_dbg=',i4,' fast_cov=',i2 )
        END IF
!
! ----- Initialization of internal data structures used by FLYBY_MAP
!
        CALL FLYBY_MAP_INIT()
!
! ----- Calculation of coefficients of cubic spline for interpolation high
! ----- frequency EOP
!
        CALL ERR_PASS ( IUER, IER )
        CALL HFINT_INIT ( IER )
        IF ( IER .NE. 0 ) THEN
             CALL ERR_LOG ( 8334, IUER, 'LOOP', 'Error during '// &
     &           'attempt to build coefficients of cubic spline for '// &
     &           'interpolation of high frequency EOP' )
             RETURN
        END IF
!
! ----- Clearing of all internal buffers of B3DOBJ, B1B3DOBJ and PLACE
!
        CALL F__CLR_IND ( 3, FAST_MODE, PLACE, B3DOBJ, B1B3DOBJ )
!
! ----- Determine whether or not necassary to take into account rates in normal
! ----- equations
!
        IF ( DATYP_INQ ( IDATYP, RATE__DTP ) ) THEN
             PLACE%STATUS = F__RAT
           ELSE
             PLACE%STATUS = F__DEL
        END IF
!
! ----- Initialization of the statistics counters
!
        CALL ERR_PASS ( IUER, IER )
        CALL SESTAT_INIT ( DBOBJ, 1, B3DOBJ%DBNAME_MES, IDATYP, IER )
        IF ( IER .NE. 0 ) THEN
             CALL ERR_LOG ( 8335, IUER, 'LOOP', 'Error during '// &
     &           'attempt to initilize data structures for collecting '// &
     &           'statistics of the database '//B3DOBJ%DBNAME_MES )
             RETURN
        END IF
!
! ----- Initilialzation
!
        WAS_FIRST = .FALSE.
        WW_ALL = 0.0
        JDATE_ALL_BEG = -BIG_VALUE
        JDATE_ALL_MID = 0.0D0
        ADDW_EXT = 0.0D0
        ADDW_FRQ = 0.0D0
        FL_ADDW_IONO = .FALSE.
        FL_ADDW_BW   = .FALSE.
!
        DO 410 J1=1,NUMSTA
           WW_STA(J1) = 0.0D0
           JDATE_STA_BEG(J1) = -BIG_VALUE
           JDATE_STA_MID(J1) = 0.0D0
 410    CONTINUE
!
        DO ISTA = 1, NUMSTA
           C_STA(ISTA) = ISITN_CHR(ISTA)
           CALL VTD_NAME_REPAIR ( C_STA(ISTA) )
        END DO
!
        IF ( ILEN(AOC_FIL) > 0 ) THEN
             INQUIRE ( FILE=AOC_FIL, EXIST=FL_AOC ) 
             IF ( .NOT. FL_AOC ) THEN
                  CALL ERR_LOG ( 8336, IUER, 'LOOP', 'Apriori observation '// &
     &                'correction file '//TRIM(AOC_FIL)//' does not exist' )
                  RETURN
             END IF
!
             CALL ERR_PASS ( IUER, IER )
             CALL RD_TEXT ( AOC_FIL, MAX_OBS, BUF_AOC, NA, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8337, IUER, 'LOOP', 'Error in an attempt '// &
     &                'to read the external apriori observation correction '// &
     &                'file '//EDIT_FIL )
                  RETURN
             END IF
!
             IF ( BUF_AOC(1)(1:LEN(LABEL__AOC)) .NE. LABEL__AOC ) THEN
                  CALL ERR_LOG ( 8338, IUER, 'LOOP', 'Wrong 1st line of the '// &
     &                'external a priori observation correction file '//TRIM(AOC_FIL)// &
     &                ' -- '//TRIM(BUF_AOC(1))//' while '//LABEL__AOC// &
     &                ' was expected'  )
                  RETURN
             END IF
             IF ( BUF_AOC(3)(15:24) .NE. DBNAME_CH(1:10) ) THEN
                  CALL ERR_LOG ( 8339, IUER, 'LOOP', 'Experiment name '// &
     &                'defined in external a priori observation correction file '// &
     &                TRIM(AOC_FIL)//' -- '//BUF_AOC(3)(15:24)//' does not match to the '// &
     &                'experiment being processed '//DBNAME_CH )
                  RETURN
             END IF
!
             AOC_DEL = 0.0D0
             DO 420 J2=1,NA
                IF ( BUF_AOC(J2)(1:1)   == '#' ) GOTO 420
                IF ( ILEN(BUF_AOC(J2))  ==  0  ) GOTO 420
                IF ( BUF_AOC(J2)(1:8)   == 'Ind_obs:' .AND. &
     &               BUF_AOC(J2)(17:24) == 'Add_del:'       ) THEN
                     CONTINUE 
                   ELSE
                     CALL CLRCH ( STR )
                     CALL INCH  ( J2, STR )
                     CALL ERR_LOG ( 8340, IUER, 'LOOP', 'Error in parsing '// &
     &                   'the '//TRIM(STR)//' line the external apriori '// &
     &                   'observation correction  file '//AOC_FIL )
                     RETURN
                END IF
                CALL CHIN ( BUF_AOC(J2)(10:15), IND_OBS )
                READ ( UNIT=BUF_AOC(J2)(26:38), FMT='(F13.5)', IOSTAT=IER ) AOC_DEL(IND_OBS)
                IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( J2, STR )
                     CALL ERR_LOG ( 8341, IUER, 'LOOP', 'Error in parsing '// &
     &                   'the value in the '//TRIM(STR)//' line of the external '// &
     &                   'apriori observation correction file '//AOC_FIL )
                     RETURN
                END IF
   420       CONTINUE 
           ELSE 
             FL_AOC = .FALSE.
        END IF
!
        IF ( ILEN(EDIT_FIL) > 0 ) THEN
             FL_EDIT = .TRUE.
             FL_SUP  = .FALSE.
             CALL ERR_PASS ( IUER, IER )
             CALL RD_TEXT ( EDIT_FIL, MAX_OBS, BUF_EDIT, NE, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8342, IUER, 'LOOP', 'Error in an attempt '// &
     &                'to read external edit flag file '//EDIT_FIL )
                  RETURN
             END IF
             DO 430 J3=1,NE
                IF ( BUF_EDIT(J3)(1:1) == '#' ) GOTO 430
                IF ( ILEN(BUF_EDIT(J3)) == 0  ) GOTO 430
                CALL CHIN ( BUF_EDIT(J3), IND_OBS )
                IF ( IND_OBS > 0 .AND. IND_OBS .LE. MAX_OBS ) THEN
                     FL_SUP(IND_OBS) = .TRUE.
                END IF
   430       CONTINUE 
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
                  CALL ERR_LOG ( 8343, IUER, 'LOOP', 'Error in an attempt '// &
     &                'to read external additive flag file '//ADDW_FIL )
                  RETURN
             END IF
!
             IF ( BUF_ADDW(1)(1:LEN(LABEL__ADDW)) .NE. LABEL__ADDW .AND. &
     &            BUF_ADDW(1)(1:LEN(LABEL__ADDW_V1)) .NE. LABEL__ADDW_V1    ) THEN
                  CALL ERR_LOG ( 8344, IUER, 'LOOP', 'Wrong 1st line of the '// &
     &                'external additive flag file '//TRIM(ADDW_FIL)// &
     &                ' -- '//TRIM(BUF_ADDW(1))//' while '//LABEL__ADDW// &
     &                ' was expected'  )
                  RETURN
             END IF
             IF ( BUF_ADDW(3)(15:24) .NE. DBNAME_CH(1:10) ) THEN
                  CALL ERR_LOG ( 8345, IUER, 'LOOP', 'Experiment name '// &
     &                'defined in external additive flag file '//TRIM(ADDW_FIL)// &
     &                ' -- '//BUF_ADDW(3)(15:24)//' does not match to the '// &
     &                'experiment being processed '//DBNAME_CH )
                  RETURN
             END IF
             DO 440 J4=1,N_WEI
                IF ( BUF_ADDW(J4)(1:32) == '# Average Ionospheric frequency:' ) THEN
                     READ ( UNIT=BUF_ADDW(J4)(34:46), FMT='(F13.5)' ) ADDW_FRQ
                     FL_ADDW_IONO = .TRUE.
                END IF
!
! ------------- Check the flag about honoring baseline weights. 
! ------------- If the baseline weights are "included", than the total weights 
! ------------- take into accout the baseline weights
! ------------- If the baseline weights are "independent", than the total weights 
! ------------- are a sum in quadrature of the weights in this file and the baseline weights
!
                IF ( BUF_ADDW(J4)(1:29) == '# Baseline weights: included'    ) FL_ADDW_BW = .TRUE.
                IF ( BUF_ADDW(J4)(1:31) == '# Baseline weights: independent' ) FL_ADDW_BW = .FALSE.
                IF ( BUF_ADDW(J4)(1:1)  == '#' ) GOTO 440
                IF ( ILEN(BUF_ADDW(J4)) ==  0  ) GOTO 440
                CALL CHIN ( BUF_ADDW(J4)(10:15), IND_OBS )
                IF ( IND_OBS > 0 .AND. IND_OBS .LE. MAX_OBS ) THEN
                     READ ( UNIT=BUF_ADDW(J4)(27:38), FMT='(D12.5)' ) ADDW_EXT(IND_OBS)
                END IF
   440       CONTINUE 
          ELSE 
             FL_ADDW_IONO = .FALSE.
             FL_ADDW = .FALSE.
             ADDW_EXT = 0.0D0
             ADDW_FRQ = 0.0D0
        END IF
!
        IF ( ILEN(DTEC_FIL) > 0 ) THEN
             FL_DTEC = .TRUE.
             CALL ERR_PASS ( IUER, IER )
             CALL RD_TEXT  ( DTEC_FIL, MAX_OBS, BUF_DTEC, N_DTEC, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8346, IUER, 'LOOP', 'Error in an attempt '// &
     &                'to read external additive flag file '//DTEC_FIL )
                  RETURN
             END IF
!
             IF ( BUF_DTEC(1)(1:LEN(LABEL__DTEC)) .NE. LABEL__DTEC ) THEN
                  CALL ERR_LOG ( 8347, IUER, 'LOOP', 'Wrong 1st line of the '// &
     &                'external dTEC file '//TRIM(DTEC_FIL)// &
     &                ' -- '//TRIM(BUF_DTEC(1))//' while '//LABEL__DTEC// &
     &                ' was expected'  )
                  RETURN
             END IF
             IF ( BUF_DTEC(8)(15:24) .NE. DBNAME_CH(1:10) ) THEN
                  CALL ERR_LOG ( 8348, IUER, 'LOOP', 'Experiment name '// &
     &                'defined in external dTEC file '//TRIM(DTEC_FIL)// &
     &                ' -- '//BUF_DTEC(8)(15:24)//' does not match to the '// &
     &                'experiment being processed '//DBNAME_CH )
                  RETURN
             END IF
             DTEC_GPS_EXT    = 0.0D0
             DTEC_ADJ_EXT    = 0.0D0
             DTEC_ERR_EXT    = 0.0D0
             DEL_BIAS_UL_EXT = 0.0D0
             DO 450 J5=1,N_DTEC
                IF ( BUF_DTEC(J5)(1:1) == '#' ) GOTO 450
                IF ( ILEN(BUF_DTEC(J5)) == 0  ) GOTO 450
                CALL CHIN ( BUF_DTEC(J5)(10:15), IND_OBS )
                IF ( IND_OBS > 0 .AND. IND_OBS .LE. MAX_OBS ) THEN
                     READ ( UNIT=BUF_DTEC(J5)(27:36),   FMT='(F10.2)' ) DTEC_GPS_EXT(1,IND_OBS)
                     READ ( UNIT=BUF_DTEC(J5)(38:47),   FMT='(F10.2)' ) DTEC_GPS_EXT(2,IND_OBS)
                     READ ( UNIT=BUF_DTEC(J5)(60:69),   FMT='(F9.3)'  ) DTEC_ADJ_EXT(1,IND_OBS)
                     READ ( UNIT=BUF_DTEC(J5)(70:78),   FMT='(F9.3)'  ) DTEC_ADJ_EXT(2,IND_OBS)
                     READ ( UNIT=BUF_DTEC(J5)(91:98),   FMT='(F8.3)'  ) DTEC_ERR_EXT(IND_OBS)
                     READ ( UNIT=BUF_DTEC(J5)(111:123), FMT='(D13.6)' ) DEL_BIAS_UL_EXT(IND_OBS)
                     READ ( UNIT=BUF_DTEC(J5)(153:161), FMT='(F9.3)'  ) DTEC_VMG(IND_OBS)
                     READ ( UNIT=BUF_DTEC(J5)(173:181), FMT='(F9.3)'  ) DTEC_RES(IND_OBS)
                     READ ( UNIT=BUF_DTEC(J5)(195:203), FMT='(F9.3)'  ) VIONO_ERR(IND_OBS)
                END IF
   450       CONTINUE 
           ELSE
             FL_DTEC = .FALSE.
             DTEC_GPS_EXT    = 0.0D0
             DTEC_ADJ_EXT    = 0.0D0
             DTEC_ERR_EXT    = 0.0D0
             DEL_BIAS_UL_EXT = 0.0D0
        END IF
        IF ( ILEN(EXT_ERR_FIL) > 0 ) THEN
             FL_EXT_ERR = .TRUE.
             CALL ERR_PASS ( IUER, IER )
             CALL RD_TEXT  ( EXT_ERR_FIL, MAX_OBS, BUF_EXT_ERR, N_EXT_ERR, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8349, IUER, 'LOOP', 'Error in an attempt '// &
     &                'to read external errir flag file '//EXT_ERR_FIL )
                  RETURN
             END IF
!
             IF ( BUF_EXT_ERR(1)(1:LEN(LABEL__EXT_ERR)) .NE. LABEL__EXT_ERR ) THEN
                  CALL ERR_LOG ( 8350, IUER, 'LOOP', 'Wrong 1st line of the '// &
     &                'external error file file '//TRIM(EXT_ERR_FIL)// &
     &                ' -- '//TRIM(BUF_EXT_ERR(1))//' while '//LABEL__EXT_ERR// &
     &                ' was expected'  )
                  RETURN
             END IF
             IF ( BUF_EXT_ERR(3)(15:24) .NE. DBNAME_CH(1:10) ) THEN
                  CALL ERR_LOG ( 8351, IUER, 'LOOP', 'Experiment name '// &
     &                'defined in external external error file '//TRIM(EXT_ERR_FIL)// &
     &                ' -- '//BUF_EXT_ERR(3)(15:24)//' does not match to the '// &
     &                'experiment being processed '//DBNAME_CH )
                  RETURN
             END IF
             EXT_ERR = 0.0D0
             DO 460 J6=1,N_EXT_ERR
                IF (      BUF_EXT_ERR(J6)(1:1) == '#' ) GOTO 460
                IF ( ILEN(BUF_EXT_ERR(J6))     ==  0  ) GOTO 460
                CALL CHIN ( BUF_EXT_ERR(J6)(10:15), IND_OBS )
                IF ( IND_OBS > 0 .AND. IND_OBS .LE. MAX_OBS ) THEN
                     READ ( UNIT=BUF_EXT_ERR(J6)(26:38), FMT='(F13.6)' ) EXT_ERR(IND_OBS)
                END IF
 460         CONTINUE 
           ELSE
             FL_EXT_ERR = .FALSE.
        END IF
!
        IF ( EDC_USE == EDC__CRE ) THEN
!
! ---------- Initialize external decimation file
!
             CALL ERR_PASS  ( IUER, IER )
             CALL EDC_INIT  ( EDC, IDEND-IDBGN+1, IER  )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8352, IUER, 'LOOP', 'Error in an attempt '// &
     &                'to create external decimation file' )
                  RETURN
             END IF
          ELSE IF ( EDC_USE == EDC__REQ .OR. &
     &              EDC_USE == EDC__USE      ) THEN
!
! ---------- Read external decimation file
!
             IF ( EDC_USE == EDC__REQ ) CALL ERR_PASS ( IUER, IER )
             IF ( EDC_USE == EDC__USE ) CALL ERR_PASS ( 0,    IER )
             CALL EDC_READ ( DBNAME_CH, EDC_DIR, EDC, IER )
             IF ( IER .NE. 0  .AND. EDC_USE == EDC__REQ ) THEN
                  CALL ERR_LOG ( 8353, IUER, 'LOOP', 'Error in an attempt '// &
     &                'to open external decimation file' )
                  RETURN
             END IF
             WRITE ( 23, '(A)' ) 'PROC: Decimation for database '//DBNAME_CH// &
     &                           ' Procedure '//EDC%HEA%PRC_NAME(1:I_LEN(EDC%HEA%PRC_NAME))
        END IF
!
        DO NOBS = IDBGN, IDEND ! Running over the observations!
           KOBS = NOBS - IDBGN + 1  ! Number of the observation counted form 1
!           write ( 6, * ) ' PROC(loop)-1071  NOBS= ', NOBS, ' IDEND= ', IDEND ; call flush ( 6 ) ! %%%
           IF ( K_MN  .AND.  NOBS .EQ. IDBGN ) THEN
!
! ----------- Initial printout in interactive mode
!
              CALL SETCR_MN ( 0, 1 )
              WRITE  ( BUFSTR, 60 ) B3DOBJ%DBNAME_MES, IDEND - IDBGN + 1
 60           format ( "Making normal equations for database  ",a, &
     &                 "   0   (", i8, " )" )
              CALL ADDSTR_F ( BUFSTR(:73) )
              CALL SETCR_MN ( 78, 1 )
              CALL REFRESH_MN()
           END IF
!
           IF ( K_MN .AND. ( MOD(KOBS,5000) .EQ. 0  .OR.  NOBS .EQ. IDEND ) ) THEN
!
! ------------- Printout in interactive mode
!
                CALL SETCR_MN ( 52, 1 )
                WRITE ( BUFSTR, '(I8)'  ) KOBS
                CALL ADDSTR_F ( BUFSTR(:8) )
                CALL SETCR_MN ( 74, 1 )
                CALL REFRESH_MN()
           END IF
!
! -------- Read the next record from OBSFIL
!
           CALL USE_OBSFIL ( IOBSFIL, NOBS, 'R' )
           IF ( IS_R8_NAN(RERR) ) THEN
                RERR = 1.D-10
                CALL USE_OBSFIL ( IOBSFIL, NOBS, 'W' )
           ENDIF
           FL_OBSFIL_UPDATE = .FALSE.
           IF ( NOBS == 1 ) THEN
                TMIN = FJD + FRACTC
           END IF
           IF ( SOLVE_DEBUG == 'PX' ) THEN
                DPH    = TOTPH*DEG__TO__RAD/(PI2*REFFREQ)
                NPHAM4 = (DOBS/1.D6 - DPH)/(PHAMI8*1.D-6)
                DPH    = 1.D6*(DPH + NPHAM4*(PHAMI8*1.D-6))
                FL_OBSFIL_UPDATE = .TRUE.
           END IF
           IF ( SOLVE_DEBUG == 'PS' ) THEN
                DPHXS    = TOTPH_S*DEG__TO__RAD/(PI2*REFFREQ_S)
                NPHAM4_S = (DOBSXS/1.D6 - DPHXS)/(PHAMI8_S*1.D-6)
                DPHXS    = 1.D6*(DPHXS + NPHAM4_S*(PHAMI8_S*1.D-6))
                FL_OBSFIL_UPDATE = .TRUE. 
           END IF
!          write ( 6, * ) ' PROC(loop)-1361  NOBS= ', NOBS ; call flush ( 6 ) ! %%%
           IF ( SOLVE_DEBUG(1:1) == 'Q' ) THEN
!
! ------------- Reset AUTO suppression flags
!
                AUTO_SUP = 0
                AUTO_SUP = IBSET ( AUTO_SUP, INT4(INIT__SPS) )
                IF ( LQUAL_CHR == ' 9' .OR. LQUAL_CHR == ' 8' .OR. &
     &               LQUAL_CHR == ' 7' .OR. LQUAL_CHR == ' 6'      ) THEN
                     CONTINUE
                   ELSE
                     IF ( LQUAL_CHR == ' 5' .OR. LQUAL_CHR == ' 4' .OR. &
     &                    LQUAL_CHR == ' 4' .OR. LQUAL_CHR == ' 2' .OR. &
     &                    LQUAL_CHR == ' 1'                             ) THEN
                          AUTO_SUP = IBSET ( AUTO_SUP, INT4(BQCX__SPS) )
                       ELSE
                          AUTO_SUP = IBSET ( AUTO_SUP, INT4(NOFX__SPS) )
                     END IF
                END IF
                IF ( LQUAL_S_CHR == ' 9' .OR. LQUAL_S_CHR == ' 8' .OR. &
     &               LQUAL_S_CHR == ' 7' .OR. LQUAL_S_CHR == ' 6'      ) THEN
                     CONTINUE 
                   ELSE
                     IF ( LQUALXS_CHR == ' 5' .OR. LQUAL_CHR == ' 4' .OR. &
     &                    LQUALXS_CHR == ' 4' .OR. LQUAL_CHR == ' 2' .OR. &
     &                    LQUALXS_CHR == ' 1'                             ) THEN
                          AUTO_SUP = IBSET ( AUTO_SUP, INT4(BQCS__SPS) )
                       ELSE
                          AUTO_SUP = IBSET ( AUTO_SUP, INT4(NOFS__SPS) )
                     END IF
                END IF
!
                IF ( ELEV(1) .LT. ELVCUT(ISITE(1)) ) AUTO_SUP = IBSET ( AUTO_SUP, INT4(CUEL__SPS) )
                IF ( ELEV(2) .LT. ELVCUT(ISITE(2)) ) AUTO_SUP = IBSET ( AUTO_SUP, INT4(CUEL__SPS) )
!
                IF ( DATYP_INQ ( IDATYP, SBAND__DTP ) .OR. &
      &              DATYP_INQ ( IDATYP, COMB__DTP  ) .OR. &
      &              FL_USED_AS_XS                         ) THEN
                     IF ( ILEN(ENV_FINAM) > 0 ) THEN
                          IF ( SNR_MIN_S > 0.0  .AND.  SNR_S < SNR_MIN_S  .AND.  &
      &                        KBIT ( OPP_STATUS, OPP_SET1__BIT  )  ) THEN
                               AUTO_SUP = IBSET ( AUTO_SUP, INT4(LSNR__SPS) )
                             ELSE 
                               AUTO_SUP = IBCLR ( AUTO_SUP, INT4(LSNR__SPS) )
                          END IF 
                     END IF 
                END IF 
                IF ( DATYP_INQ ( IDATYP, FUSED__DTP ) ) THEN
                     IF ( ( SNR_MIN_X > 0.0  .AND.  SNR   < SNR_MIN_X ) .AND. &
      &                   ( SNR_MIN_S > 0.0  .AND.  SNR_S < SNR_MIN_S )       ) THEN
                           AUTO_SUP = IBSET ( AUTO_SUP, INT4(LSNR__SPS) )
                        ELSE               
                           AUTO_SUP = IBCLR ( AUTO_SUP, INT4(LSNR__SPS) )
                     END IF
                END IF 
!
                FL_OBSFIL_UPDATE = .TRUE.
           END IF
!
           IF ( SOLVE_DEBUG(1:1) == 'I'  .AND.  SUPMET == SUPMET__META ) THEN
!
! ------------- Special kludge for resetting suppression status to
! ------------- "nothing is suppressed". It is supposed to work
! ------------- under META suppression method only
!
                IUNW = 0
                CALL SBIT ( SUPSTAT,  IUNW__SPS+1,  0 )
                CALL SBIT ( AUTO_SUP, IUNW__SPS+1,  0 )
                CALL SBIT ( USER_SUP, IDATYP+1,     0 )
                CALL SBIT ( USER_SUP, G_GXS__DTP+1, 0 )
                CALL SBIT ( USER_SUP, GX__DTP+1,    0 )
                CALL SBIT ( USER_SUP, GS__DTP+1,    0 )
                CALL SBIT ( USER_SUP, GOOD__SPS+1,  0 )
                CALL SBIT ( USER_SUP, CBAD__SPS+1,  0 )
                FL_OBSFIL_UPDATE = .TRUE.
              ELSE IF ( SOLVE_DEBUG(1:1) .NE. 'I' .AND. &
     &                  ILEN(ENV_FINAM) > 0       .AND. &
     &                  ( SNR_MIN_X > 0.0 .OR. SNR_MIN_S > 0.0 ) ) THEN
!
! ------------- Update Low SNR status bits on the fly in non-interactive solution
!
                IF ( DATYP_INQ ( IDATYP, FUSED__DTP  ) ) THEN
                     IF ( ( SNR_MIN_X > 0.0  .AND.  SNR   < SNR_MIN_X ) .AND. &
     &                    ( SNR_MIN_S > 0.0  .AND.  SNR_S < SNR_MIN_S )       ) THEN
!
                          IF ( BTEST ( AUTO_SUP, INT4(LSNR__SPS) ) ) FL_OBSFIL_UPDATE = .TRUE.
                          CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(1) )
                          AUTO_SUP = IBSET ( AUTO_SUP, INT4(LSNR__SPS) )
                          USER_SUP = IBSET ( USER_SUP, INT4(IDATYP) )
                        ELSE               
                          IF ( .NOT. BTEST ( AUTO_SUP, INT4(LSNR__SPS) ) ) FL_OBSFIL_UPDATE = .TRUE.
                          CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(0) )
                          AUTO_SUP = IBCLR ( AUTO_SUP, INT4(LSNR__SPS) )
                     END IF
                   ELSE
                     IF ( SNR_MIN_X > 0.0 .AND. SNR < SNR_MIN_X ) THEN
                          CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(1) )
                        ELSE 
                          CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(0) )
                     END IF 
                     IF ( SNR_MIN_S > 0.0  .AND.  SNR_S < SNR_MIN_S  .AND.  &
     &                    KBIT ( OPP_STATUS, OPP_SET1__BIT  )  ) THEN
                          CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(1) )
                        ELSE 
                          CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(0) )
                     END IF 
                END IF
             ELSE IF ( SOLVE_DEBUG(1:1) == 'J' ) THEN
               CALL META_RESET ()
               FL_OBSFIL_UPDATE = .TRUE.
           END IF
           IF ( SOLVE_DEBUG(1:9) == 'AMB_RESET' ) THEN
                DOBS   = DOBS - FAMB*1.D6*NUMAMB
                DOBSXS = DOBSXS - FAMB*1.D6*NUMAMB_S
                NUMAMB   = 0
                NUMAMB_S = 0
                FL_OBSFIL_UPDATE = .TRUE.
           END IF
           IF ( SUPMET == SUPMET__META ) THEN
                IF ( .NOT. BTEST ( AUTO_SUP, INT4(INIT__SPS) ) ) THEN
                     CALL UN_CURSES()
                     WRITE ( 6, '(A,I6,A)' ) 'Observation ', NOBS, &
     &                          ' Suppression status is not set'
                END IF
           END IF
!
           CALL JD_TO_MJD_SEC  ( FJD, MJD_UTC_OBS, UTC_OBS )
           UTC_OBS = UTC_OBS + FRACT*86400.0D0
!
           DER_DEL = 0.0D0
           IF ( ILEN(VTD_CONF_USE) > 0  .AND. .NOT. FL_TPD_READ ) THEN
!
! ------------- Load meteorological parameters of the first station
! ------------- into the VTD record
!
                PRES_VAL = ATMPR(1)*100.0D0
                TEMP_VAL = TEMPC(1) + 273.16D0
                CALL ERR_PASS ( IUER, IER )
                CALL VTD_METEO_IN ( %VAL(VTD_ADR), C_STA(ISITE(1)), &
     &                              PRES_VAL, TEMP_VAL, TEMP_VAL, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8354, IUER, 'LOOP', 'Error in an '// &
     &                   'attempt to load meteorological parameters '// &
     &                   'for station '//C_STA(ISITE(1)) )
                     RETURN
                END IF
!
! ------------- Load meteorologial parameters for the second station
!
                PRES_VAL = ATMPR(2)*100.0D0
                TEMP_VAL = TEMPC(2) + 273.16D0
                CALL ERR_PASS ( IUER, IER )
                CALL VTD_METEO_IN ( %VAL(VTD_ADR), C_STA(ISITE(2)), &
     &                              PRES_VAL, TEMP_VAL, TEMP_VAL, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8355, IUER, 'LOOP', 'Error in an '// &
     &                   'attempt to load meteorological parameters '// &
     &                   'for station '//C_STA(ISITE(2)) )
                     RETURN
                END IF
                TAU_ACM  = 0.0D0
                RATE_ACM = 0.0D0
!
! ------------- NB: we need to transform FJD+FRACTC by parts, otherwise
! ------------- we will lose precision whcih will result in an additional
! ------------- noise in delay with rms 10-15 ps!!!
!
                IF ( NOBS == 1 ) THEN
!
! ------------------ This trick is done since VLBI formatter stores pseudo-UTC.
! ------------------ We need to record UTC-TAI(t) at the beginning of the
! ------------------ experiment and apply it to all observations, regardless
! ------------------ whether the new clock jump took place duing the experiment
!
                     CALL VTD_UTC_TO_TAI ( %VAL(VTD_ADR), MJD_UTC_OBS, &
     &                                     UTC_OBS, TAI_OBS, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 8356, IUER, 'LOOP', 'Error in an '// &
     &                         'attempt to get UTC-minus TAI' )
                          RETURN
                     END IF
                     UTC_MINUS_TAI = UTC_OBS - TAI_OBS
                     IF ( UTC_MINUS_TAI >  43200.0D0 ) UTC_MINUS_TAI = UTC_MINUS_TAI - 86400.0D0
                     IF ( UTC_MINUS_TAI < -43200.0D0 ) UTC_MINUS_TAI = UTC_MINUS_TAI + 86400.0D0
                END IF
                UTC_M_TAI = UTC_MINUS_TAI
!  
                TAI_OBS = UTC_OBS - UTC_M_TAI
                IF ( TAI_OBS < 0.0D0 ) THEN
                     TAI_OBS = TAI_OBS + 86400.0D0
                     MJD_TAI_OBS = MJD_UTC_OBS - 1
                   ELSE
                     MJD_TAI_OBS = MJD_UTC_OBS
                END IF
                IF ( NOBS == 1 ) THEN
                     TAI_1ST = TAI_OBS
                     MJD_1ST = MJD_TAI_OBS
                END IF
!
! ------------- Set fields of OBS_TYP
!      
                IF ( GIM_COLLECT_INFO .OR. SOLVE_DEBUG(1:8) == 'PUT_IONO' ) THEN
                     IDATYP_SAVE = IDATYP
                     IDATYP = GX__DTP
                END IF
                CALL SET_OBSTYP ( OBS_TYP )
                IF ( GIM_COLLECT_INFO .OR. SOLVE_DEBUG(1:8) == 'PUT_IONO' ) THEN
                     IDATYP = IDATYP_SAVE 
                END IF
!
                CALL ERR_PASS ( IUER, IER )
                CALL VTD_DELAY ( ISTRN_CHR(ISTAR), C_STA(ISITE(1)), &
     &                           C_STA(ISITE(2)), MJD_TAI_OBS, TAI_OBS, &
     &                           OBS_TYP, %VAL(VTD_ADR), DELAY_THR, &
     &                           RATE_THR, DER_DEL, DER_RAT, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8357, IUER, 'LOOP', 'Error in an '// &
     &                   'attempt to compute VLBI time delay' )
                     RETURN
                END IF
                IONO_GPS = DER_DEL(VTD__IONO2) - DER_DEL(VTD__IONO1)
                IF ( GIM_COLLECT_INFO .OR. SOLVE_DEBUG(1:8) == 'PUT_IONO' ) THEN
                     DELAY_THR = DELAY_THR - IONO_GPS
                END IF
!
                CALL ERR_PASS ( IUER, IER )
                CALL POST_VTD ( NOBS, %VAL(VTD_ADR), TRP_USE, %VAL(ADR_TRP), &
     &                          STS_TRP, DELAY_THR, RATE_THR, DER_DEL, &
     &                          DER_RAT, FL_NO_IONO_DEL, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8358, IUER, 'LOOP', 'Error in an '// &
     &                   'attempt to collect results of VTD' )
                     RETURN
                END IF
!
                IF ( L_ACM > 0 ) THEN
!
! ------------------ Initializing contribution of a priori clock model
!
                     IF ( L_ACM > 0 ) THEN
!
! ----------------------- Adding contributions from a priori clok model to theoretical
! ----------------------- time delay and delay rate
!
! -----------------------  TT  -- proper time of the first station from TMIN epoch to the moment
! -----------------------         of observation (in sec)
! -----------------------  TT2 -- proper time of the second station from TMIN epoch to the
! -----------------------         moment of observation (in sec)
!
                           TT  = ((FJD - TMIN) + FRACTC)*86400.0D0
                           TT2 = TT + DT*1.D-6
!
! -----------------------  IP1 -- index of the first station of the baseine in
! -----------------------         the list of stations for which a priori clock model
! -----------------------         has been applied
!
                          IP1 = LTM_DIF ( 0, L_ACM, STAT_ACM, ISITN_CHR(ISITE(1)) )
                          IF ( IP1 .GT. 0 ) THEN
!
! ---------------------------  New correction of theoreticals due to up to date ACM
!
                               TAU_ACM  = TAU_ACM  - CLOOF_ACM(IP1) - CLODR_ACM(IP1)*TT
                               RATE_ACM = RATE_ACM - CLODR_ACM(IP1)
                          END IF
!
! ----------------------- The same for the second station. But NB sign!
!
                          IP2 = LTM_DIF ( 0, L_ACM, STAT_ACM, ISITN_CHR(ISITE(2)) )
                          IF ( IP2 .GT. 0 ) THEN
                               TAU_ACM  = TAU_ACM  + CLOOF_ACM(IP2) + CLODR_ACM(IP2)*TT2
                               RATE_ACM = RATE_ACM + CLODR_ACM(IP2)
                          END IF
!
! ----------------------- Correction for theoreticals. NB units for DT!
!
                          DT = ( DT*1.D-6 + TAU_ACM  ) *1.D6
                          RT = ( RT       + RATE_ACM )
                     END IF
                END IF
                IF ( .NOT. KBATCH ) THEN
                     FL_OBSFIL_UPDATE = .TRUE.
                END IF
              ELSE IF ( .NOT. FL_TPD_READ ) THEN
!
! ------------- Case when we do not use VTD
!
!
                IF ( NOBS == 1 ) THEN
                     CALL ERR_PASS ( IUER, IER )
                     CALL GET_UTC_M_TAI ( NERS, MJD_UTC_OBS, UTC_OBS, UTC_M_TAI, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 8359, IUER, 'LOOP', 'Error in '// &
     &                        'an attempt to compute function UTC-minus-TAI' )
                          RETURN
                     END IF
                END IF
                TAI_OBS = UTC_OBS - UTC_M_TAI
                IF ( TAI_OBS < 0.0D0 ) THEN
                     TAI_OBS = TAI_OBS + 86400.0D0
                     MJD_TAI_OBS= MJD_UTC_OBS - 1
                   ELSE
                     MJD_TAI_OBS= MJD_UTC_OBS
                END IF
           END IF
!
           IF ( FL_TPD_READ ) THEN
                IF ( NOBS == 1 ) THEN
                     CALL USE_COMMON ( 'OR' )
                     CALL USE_GLBFIL ( 'OR' )
                     CALL USE_PARFIL ( 'OR' )
                     CALL SOCOM_EXT()
                END IF
                CALL ERR_PASS ( IUER, IER )
                CALL TPD_GET  ( TPD, NOBS, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8360, IUER, 'LOOP', 'Failure during '// &
     &                   'attempt to get TPD data structure while '// &
     &                   'experiment '//B3DOBJ%DBNAME_MES//' was beging '// &
     &                   'processing' )
                     RETURN
                END IF
!
                IF ( NOBS == 1 ) THEN
!
! ------------------ We do it in order to save 5 varaibles read from the
! ------------------ header of TPD file
!
                     CALL USE_COMMON ( 'WC' )
                     CALL USE_GLBFIL ( 'WC' )
                     CALL USE_PARFIL ( 'WC' )
                END IF
           END IF
!
           IF ( FL_TPD_WRITE ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL TPD_PUT  ( TPD, NOBS, IER )
                IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 8361, IUER, 'LOOP', 'Failure during '// &
     &                  'attempt to put TPD data structure while '// &
     &                  'experiment '//B3DOBJ%DBNAME_MES//' was beging '// &
     &                  'processing' )
                    RETURN
                END IF
           END IF
!
! -------- Getting date of the first and the current observation
!
           IF ( KOBS .EQ. 1 ) B3DOBJ%JD_NOM_FIRST = FJD + FRACTC
           B3DOBJ%JD_NOM_LAST                     = FJD + FRACTC
!
           IF ( FL_EDIT ) THEN
                IF ( FL_SUP(NOBS) ) THEN
                     AUTO_SUP = IBSET ( AUTO_SUP, INT4(EXTS__SPS) )
                   ELSE 
                     IF ( .NOT. BTEST ( AUTO_SUP, INT4(EXTS__SPS) ) ) THEN
                          USER_SUP = IBCLR ( USER_SUP, INT4(IDATYP) )
                     END IF
                END IF
           END IF
!
! -------- Lifting suppression flag for simulation runs
!
           IF ( SIMULATION_TEST .AND. (ALL_SIM_FLG .OR. KBATCH) ) THEN
                IUNW = 0
           ENDIF
!
           IF ( ISITE(1) .LE. 0   .OR.  ISITE(2) .LE. 0 ) THEN
                WRITE ( 6, * ) ' before flyby isite(1)=',isite(1), &
     &          ' isite(2)=',isite(2)
                CALL ERR_LOG ( 8362, IUER, 'LOOP', 'Error during '// &
     &              'analysis of '//B3DOBJ%DBNAME_MES//' : some scratch '// &
     &              'files should be updated' )
                RETURN
           END IF
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
!
           IF ( FAST_MODE .EQ. F__B3D  .OR.  FAST_MODE .EQ. F__B1B3D  ) THEN
!
! ------------- Store corrected DT and RT (theoretical delay and rate)
!
                B3DOBJ%FIRST_FIELD = INT4(IDB)
                B3DOBJ%DT(KOBS) = DT
                B3DOBJ%RT(KOBS) = RT
              ELSE
                B3DOBJ%FIRST_FIELD = 0
           END IF
 500       CONTINUE
!!          write ( 6, * ) ' PROC(loop)-1753  NOBS= ', NOBS, ' DOBS= ', DOBS ; call flush ( 6 ) ! %%%
!
! -------- Preparation data for calibration
!
           TAU_CALC     = DT/1.D6
           RATE_CALC    = RT
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
! -------- Preserve original group deley error and delay reweigting.
! -------- NB: this will work in META suppression mode only
!
           TAUGR_X_ERR_ORIG = TAUGR_ERR_X
           TAUGR_S_ERR_ORIG = TAUGR_ERR_S
           N_VAR = MAX_ARC_STA - 2
           IF ( ITT(ISITE(1)) < ITT(ISITE(2)) ) THEN
                J_VAR = ((ITT(ISITE(1))-1) * N_VAR) - (((ITT(ISITE(1))-1) * (ITT(ISITE(1))-2))/2) + &
     &                    ITT(ISITE(2))-1
              ELSE
                J_VAR = ((ITT(ISITE(2))-1) * N_VAR) - (((ITT(ISITE(2))-1) * (ITT(ISITE(2))-2))/2) + &
     &                    ITT(ISITE(1))-1
           END IF
           JJ_VAR = ITTB(J_VAR)
           IF ( JJ_VAR .GE. 1  .AND. JJ_VAR .LE. MAX_ARC_BSL ) THEN
                TAU_REWEI = ET(1,JJ_VAR)
              ELSE
                TAU_REWEI = 0.0D0
           END IF
!
           IF ( SOLVE_DEBUG == 'DISCARD_S' ) THEN
                TAUGR_OBS_S  = 0.0D0
                TAUPH_OBS_S  = 0.0D0
                TAUSB_OBS_S  = 0.0D0
                TAUGR_ERR_S  = 0.0D0
                TAUPH_ERR_S  = 0.0D0
                TAUSB_ERR_S  = 0.0D0
           END IF
!
           IF ( IS_R8_NAN ( FREQ_GR_X ) ) THEN
!
! ------------- Not a number! O-o-o! Set a bogus value to prevent operations
! ------------- with Not-A-Numbers
!
                FREQ_GR_X = 8.2D9
           END IF
           IF ( IS_R8_NAN ( FREQ_GR_S ) ) THEN
!
! ------------- Not a number! O-o-o! Set a bogus value to prevent operations
! ------------- with Not-A-Numbers
!
                FREQ_GR_S = 2.2D9
           END IF
!
           IF ( IS_R8_NAN ( FREQ_PH_X ) ) THEN
!
! ------------- Not a number! O-o-o! Set a bogus value to prevent operations
! ------------- with Not-A-Numbers
!
                FREQ_PH_X = 8.2D9
           END IF
           IF ( IS_R8_NAN ( FREQ_PH_S ) ) THEN
!
! ------------- Not a number! O-o-o! Set a bogus value to prevent operations
! ------------- with Not-A-Numbers
!
                FREQ_PH_S = 2.2D9
           END IF
           IF ( DATYP_INQ ( IDATYP, FUSED__DTP ) .AND. &
     &          FREQ_GR_X < FREQ__SOLVE_MIN  .OR. FREQ_GR_S < FREQ__SOLVE_MIN ) THEN
                FREQ_GR_X    = MEAN_EFF_FREQ(1)*1.D6
                FREQ_GR_S    = MEAN_EFF_FREQ(2)*1.D6
           END IF
           IF ( IS_R8_NAN ( DERR ) ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( NOBS, STR )
                CALL ERR_LOG ( 8363, IUER, 'LOOP', 'Observation '//TRIM(STR)// &
     &              ' in '//B3DOBJ%DBNAME_MES//' has DERR NaN' )
                RETURN
           END IF
!
! -------- Correct possibly wrong formal errors
!
           IF ( DERR  .GT. TAU_ERR__BAD  ) DERR  = TAU_ERR__BAD
           IF ( DERR  .LT. TAU_ERR__TINY ) DERR  = TAU_ERR__TINY
           IF ( DPHER .GT. TAU_ERR__BAD  ) DPHER = TAU_ERR__BAD
           IF ( DPHER .LT. TAU_ERR__TINY ) DPHER = TAU_ERR__TINY
!
! -------- Saving some variables which will be sploiled by GET_CALIB
!
           DT_SAVE    = DT
           RT_SAVE    = RT
           DERR_SAVE  = DERR
           RERR_SAVE  = RERR
           DPHER_SAVE = DPHER
!
! -------- Zeroing some variables. They will be modified (calibrated) by
! -------- GET_CALIB. As a result they will accumulate all calibrations imposed
! -------- by GET_CALIB. These trick is done to maintain compatibility with
! -------- old versions.
!
           DT    = 0.0D0
           RT    = 0.0D0
           ADDERR_GR_TAU = 0.0D0
           IF ( IONO_ERR_FCT > 0.0D0 .OR. ( FL_VTD_IONO .AND. L_TCN > 0 ) ) THEN
                STA_NAM(1) = ISITN_CHR(ISITE(1))
                STA_NAM(2) = ISITN_CHR(ISITE(2))
                CALL VTD_NAME_REPAIR ( STA_NAM(1) )
                CALL VTD_NAME_REPAIR ( STA_NAM(2) )
!
                CALL ERR_PASS ( IUER, IER )
                CALL GET_IONO_AVR_COV ( %VAL(VTD_ADR), &
     &                   ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                   IONO_ZEN_AVR, IONO_ZEN_COV, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8364, IUER, 'LOOP', 'Error in an '// &
     &                   'attempt to extract ionosphere path delay '// &
     &                   'statistics' )
                     RETURN
                END IF
                DEL_GPS_IONO(1) = DER_DEL(VTD__IONO1)
                DEL_GPS_IONO(2) = DER_DEL(VTD__IONO2)
                CALL APPLY_TCN ( L_TCN, TCN, STA_NAM, IONO_ZEN_AVR, &
     &                           DEL_GPS_IONO, FREQ_GR_X, ELEV, &
     &                           DER_DEL, ADDERR_GR_TAU )
              ELSE
                IONO_ZEN_COV = 0.0D0
           END IF
!
           IF ( INDEX ( '123456789', LQUAL_CHR(2:2)    ) .GT. 0  .OR. &
     &          INDEX ( '123456789', LQUALXS_CHR(2:2)  ) .GT. 0  .OR. &
     &          SUPMET .EQ. SUPMET__PRE91                             ) THEN
!
! ------------- Go here if the quality code is larger then zero at
! ------------- least at one band. Remind: LQUALXS_CHR = '  ' means merey the
! ------------- fact that we don't have information about the quality code at
! ------------- the opposite band.
!
! ------------- Remind: NO FRINGE observations are allowed in SUPEMET__PRE91
! ------------- mode.
!
! ------------- Calculation different mapping functions for using them in
! ------------- partials on troposphere delay in zenith direction
!
                CALL ATMPART ( ITT, ISITE, ISITN, ISTAR, VSTARC, &
     &                         AZ, ELEV, ATMPR, RELHU, TEMPC, LATS, HEIGHTS, &
     &                         AX_OFFS, AX_TYPES, BARO_CALS, BARO_HEIGHTS, IDB )
!
!
! ------------- Making calibration: adding to DT (theoretical time delay)
! ------------- and to RT (theoretical delay rate) some corrections:
! ------------- 1) observation dependent contributions where requested;
! ------------- 2) non-flyby calibrations;
! ------------- 3) Apply the selected flyby calibrations:
! ------------- 4) Searching over stations and across the calibration bits in
! -------------    JCAFFL, and apply the calibrations where requested.
! -------------    Signs have been selected in SDBH
! ------------- 5) Add troposphere noise based on average atmosphere delay
! -------------    (roughly elevation dependent)
! ------------- 6) add ionosphere calibration and modify errors;
! ------------- 7) setting flag of goodness of the observation due to ionosphere
! ------------- 8) Apply reweighting constants
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
              ELSE
!
! ------------- This section is added in order to prevent work with unitilaized
! ------------- variable. In the case of the first observation of the session
! ------------- was non-detected, get_calib did not run and result was
! ------------- unpredicatble. It may trigger float point exception
!
                DERR_RAW  = -1.0D0
                RERR_RAW  = -1.0D0
                DPHER_RAW = -1.0D0
           END IF
           IF ( IS_R8_NAN( DT ) ) THEN
                CALL CLRCH ( STR )
                CALL INCH ( NOBS, STR )
                CALL FERR ( INT2(4441), 'LOOP: Observation '// &
     &               STR(1:I_LEN(STR))//' Calibration for delay is not-a-number. Probably, '// &
     &              'the database or the scratch file are corrupted', INT2(0), INT2(0) )
                STOP 'CRES  Abnormal termination'
           END IF
!
! -------- Collecting these changes
!
           COR_TAU  = DT/1.D6
           COR_RATE = RT
!
! -------- Calculation of additive weight corrections. They should be positive
! -------- or zero in the case of normal work. However it is possible situation
! -------- when this correction appeared to be imaginary. In the case when the
! -------- effective frequency is less than 5GHz, GET_CALIB consider input
! -------- DERR as a formal error of group delay at the S-band. It recalculte
! -------- DERR on the fly and output DERR is formal error of ionosphere
! -------- free linear comination of X- and S- band observables + additive
! -------- correction. DERR_RAW, DPHER_RAW, RERR_RAW kept up to now values
! -------- of formal error of ionosphere free linear combination without
! -------- applying baseline-dependent and source dependent quadratical
! -------- correction to weithts
!
           IF ( DERR - DERR_SAVE .GT. -1.D-14 ) THEN
                ADDERR_GR_TAU = DSQRT ( DABS ( DERR**2  - DERR_SAVE**2  ) )
                DERR_RAW      = DSQRT ( DERR_SAVE**2    + DERR_RAW**2  )
             ELSE
!
! ------------- Special trick to handle S-band situation. Postfix X should not
! ------------- embarass -- it means here S-band (It is true, I don't lie!!)
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
           IF ( RERR - RERR_SAVE .GT. -1.D-16 ) THEN
                ADDERR_RATE = DSQRT ( DABS ( RERR**2  - RERR_SAVE**2 ) )
                RERR_RAW    = DSQRT ( RERR_SAVE**2  + RERR_RAW**2  )
              ELSE
                ADDERR_RATE = DSQRT ( DABS ( RERR**2  - RERR_RAW**2   ) )
                RATE_ERR_X  = RERR_RAW
           END IF
           IF ( DATYP_INQ ( IDATYP, FUSED__DTP ) ) THEN
                TAUGR_ERR_X = TAUGR_X_ERR_ORIG 
                TAUGR_ERR_S = TAUGR_S_ERR_ORIG 
           END IF
!
           IF ( FL_VTD_IONO .AND. IONO_ERR_FCT > 0.0D0 ) THEN
                CONTINUE 
             ELSE IF ( FL_VTD_IONO .AND. IONO_ERR_FCT < -1.0D0 ) THEN
                IONO_VLBI = -(TAUGR_OBS_X - TAUGR_OBS_S)*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
                COR_TAU = COR_TAU + (IONO_GPS - IONO_VLBI)/IONO_ERR_FCT
           END IF
!
           TAUGR_ERR_X = SESS_REWEI_SCALE*TAUGR_ERR_X
           TAUGR_ERR_S = SESS_REWEI_SCALE*TAUGR_ERR_S
           IF ( FL_ADDW ) THEN
                ADDW_SES_SCL = ADDW_SCL
                IF ( FL_ADDW_IONO ) THEN
!
! ------------------ Case when we have additive weigth corrections due to ionospheric 
! ------------------ contribution. These additive weigtht corrections are computed 
! ------------------ at frequency ADDW_FRQ which in general not the same as 
! ------------------ the effecive ionospheric frequency of the obsevation.
! ------------------ Therefore, we scale that additive weight correction by 
! ------------------ the ratio of frequencies
!
                     IF ( DATYP_INQ ( IDATYP, XBAND__DTP ) ) THEN
                          ADDW_SES_SCL = ADDW_SCL * (ADDW_FRQ/(1.D6*EFFREQ))**2
                        ELSE IF ( DATYP_INQ ( IDATYP, SBAND__DTP ) ) THEN
                          ADDW_SES_SCL = ADDW_SCL * (ADDW_FRQ/(1.D6*EFFREQ_XS))**2
                     END IF
                END IF
!
! ------------- Apply external additive in quadrature reciprocal weight correction
! ------------- for a special case
!
!@                IF ( FL_ADDW_BW ) THEN
!@!
!@! ------------------ Baseline weights are considered included in the external weights.
!@! ------------------ The total baseline weight is the maximum among the baseline weight and
!@! ------------------ additive weight.
!@!
!@                     ADDERR_GR_TAU = MAX ( ADDW_SES_SCL*ADDW_EXT(NOBS), ADDERR_GR_TAU )
!@                   ELSE IF ( FL_ADDW_IONO ) THEN
!@!
!@! ------------------ External weights and baseline weights are considered independent
!@!
!@                     ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + (ADDW_SES_SCL*ADDW_EXT(NOBS))**2 )
!@                END IF
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
           IF ( FUSED_STATUS == IONOV__LOADED   .AND. &
     &          .NOT. FL_SUP(NOBS)              .AND. &
     &          ( DATYP_INQ ( IDATYP, FUSED__DTP  ) .OR. ILAST_OBORG_I2 == 7777 ) ) THEN
!
! ------------- Compute delay corrections and additive in quadrature uncertainty
! ------------- in the fused data type
!
                IF ( BTEST ( DTEC_FLG, DTHL__STS ) ) THEN
!
! ------------------ Both band provided data usable when the ionospheric mode was computed
!
                     CONTINUE 
                     IF ( FL_DEBUG_SBA ) write ( 6, * ) 'loop-22 DUAL: nobs= ', nobs, ' sou: ', istrn_chr(istar), ' idatyp= ', idatyp, ' cor_tau= ', sngl(cor_tau) ! %%%%
                   ELSE IF ( BTEST ( DTEC_FLG, DTH__STS ) ) THEN
!
! ------------------ Only the upper band the data usable when the ionospheric mode was computed
!
                     COR_TAU       = COR_TAU + (TEC_APR(2) - TEC_APR(1) + DTEC_ADJ)* &
     &                                         VIO__CONST/FREQ_GR_X**2 - &
     &                               DEL_BIAS_UL*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
                     ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + (DTEC_ERR*VIO__CONST/FREQ_GR_X**2)**2 )
                     IF ( FL_DEBUG_SBA ) write ( 6, * ) 'loop-22 H:    nobs= ', nobs, ' sou: ', istrn_chr(istar), ISITN_CHR(ISITE(1)), ' / ', ISITN_CHR(ISITE(2)), ' idatyp= ', idatyp, &
     &                   ' cor_tau= ', sngl(cor_tau), ' dtec_adj= ', sngl(dtec_adj), ' tec_apr= ', sngl(tec_apr), ' DEL_BIAS_UL= ', sngl(del_bias_ul*freq_gr_s**2/(freq_gr_x**2 - freq_gr_s**2)), ' err= ', sngl(dtec_err*vio__const/freq_gr_x**2) ! %%%%
                   ELSE IF ( BTEST ( DTEC_FLG, DTL__STS ) ) THEN
!
! ------------------ Only the lower band provided data usable when the ionospheric mode was computed
!
                     COR_TAU       = COR_TAU + (TEC_APR(2) - TEC_APR(1) + DTEC_ADJ)* &
     &                                         VIO__CONST/FREQ_GR_S**2 - &
     &                               DEL_BIAS_UL*FREQ_GR_X**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
                     ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + (DTEC_ERR*VIO__CONST/FREQ_GR_S**2)**2 )
                     IF ( FL_DEBUG_SBA ) write ( 6, * ) 'loop-22 L:    nobs= ', nobs, ' sou: ', istrn_chr(istar), ' idatyp= ', idatyp, ' cor_tau= ', sngl(cor_tau), ' dtec_adj= ', sngl(dtec_adj), ' tec_apr= ', sngl(tec_apr), ' DEL_BIAS_UL= ', sngl(del_bias_ul*freq_gr_x**2/(freq_gr_x**2 - freq_gr_s**2)), ' err= ', sngl(dtec_err*vio__const/freq_gr_s**2) ! %%%%
                END IF
              ELSE IF ( FUSED_STATUS == IONOV__LOADED   .AND. &
     &                  .NOT. FL_SUP(NOBS)              .AND. &
     &                  ILAST_OBORG_I2 == 8888                ) THEN
                IONO_VLBI = -(TAUGR_OBS_X - TAUGR_OBS_S)*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
                COR_TAU  = COR_TAU - (TEC_APR(2) - TEC_APR(1))* VIO__CONST/FREQ_K**2   &
     &                             + IONO_VLBI*FREQ_GR_X**2/FREQ_K**2 &
     &                             + DEL_BIAS_UL*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)*FREQ_GR_X**2/FREQ_K**2 
              ELSE IF ( FUSED_STATUS == IONOV__LOADED   .AND. &
     &                  .NOT. FL_SUP(NOBS)              .AND. &
     &                  ILAST_OBORG_I2 == 9999                ) THEN
                IONO_VLBI = -(TAUGR_OBS_X - TAUGR_OBS_S)*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
                COR_TAU  = COR_TAU - 0.784D0*(TEC_APR(2) - TEC_APR(1))* VIO__CONST/FREQ_K**2   &
     &                             + IONO_VLBI*FREQ_GR_X**2/FREQ_K**2 &
     &                             + DEL_BIAS_UL*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)*FREQ_GR_X**2/FREQ_K**2 
           END IF
!
           IF ( DATYP_INQ ( IDATYP, GX__DTP  )               .AND. &
     &          ( TEC_SCAL .NE. 0.0 .OR. TEC_BIAS .NE. 0.0 )       ) THEN
!
! ------------- Apply the contribution  of TEC scale and bias to path delay for X-band group delay observable
!
                COR_TAU = COR_TAU + ( TEC_SCAL*( TEC_APR(2)              - TEC_APR(1)              ) + &
     &                                TEC_BIAS*( DER_DEL(VTD__DER_IONO2) - DER_DEL(VTD__DER_IONO1) )   &
     &                              )* VIO__CONST/FREQ_GR_X**2 
           END IF
           IF ( DATYP_INQ ( IDATYP, GS__DTP  )               .AND. &
     &          ( TEC_SCAL .NE. 0.0 .OR. TEC_BIAS .NE. 0.0 )       ) THEN
!
! ------------- Apply the contribution  of TEC scale and bias to path delay for S-band group delay observable
!
                COR_TAU = COR_TAU + ( TEC_SCAL*( TEC_APR(2)              - TEC_APR(1)              ) + &
     &                                TEC_BIAS*( DER_DEL(VTD__DER_IONO2) - DER_DEL(VTD__DER_IONO1) )   &
     &                              )* VIO__CONST/FREQ_GR_S**2 
           END IF
!
           IF ( DTEC_SBA_USE ) THEN
!
! ------------- A special mode when the contribution of the TEC adjustment is applied
!
                IF ( DATYP_INQ ( IDATYP, GX__DTP  ) ) THEN
!
! ------------------ Upper band
!
!!                       cor_tau = cor_tau + (tec_apr(2) - tec_apr(1) + dtec_adj)* vio__const/freq_gr_x**2 
                     COR_TAU = COR_TAU + DTEC_ADJ* VIO__CONST/FREQ_GR_X**2
                     ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + (DTEC_ERR_SCL*DTEC_ERR*VIO__CONST/FREQ_GR_X**2)**2 )
                END IF
                IF ( DATYP_INQ ( IDATYP, GS__DTP  ) ) THEN
!
! ------------------ Low band
!
                     COR_TAU = COR_TAU + DTEC_ADJ* VIO__CONST/FREQ_GR_S**2 
                     ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + (DTEC_ERR_SCL*DTEC_ERR*VIO__CONST/FREQ_GR_S**2)**2 )
                END IF
           END IF
!
           IF ( ILAST_OBORG_I2 == 6666  ) THEN
                COR_TAU  = COR_TAU + DTEC_ADJ
                ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + DTEC_ERR**2 )
           END IF
           IF ( ILAST_OBORG_I2 == 7777  ) THEN
                IDATYP_SAVE = IDATYP
                IDATYP = G_GXS__DTP 
                FL_USED_GXS = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                IDATYP = IDATYP_SAVE
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
! -------- Making O-C
!
           CALL MAKE_OC ( TAU_CALC, RATE_CALC, COR_TAU, &
     &          COR_RATE, ADDERR_GR_TAU, ADDERR_PH_TAU, ADDERR_RATE, &
     &          TAUGR_OBS_X, TAUGR_OBS_S, TAUPH_OBS_X, TAUPH_OBS_S, &
     &          TAUSB_OBS_X, TAUSB_OBS_S, TAUGR_ERR_X, TAUGR_ERR_S, &
     &          TAUPH_ERR_X, TAUPH_ERR_S, TAUSB_ERR_X, TAUSB_ERR_S, &
     &          RATE_OBS_X,  RATE_OBS_S,  RATE_ERR_X,  RATE_ERR_S, &
     &          FREQ_GR_X,   FREQ_GR_S,   FREQ_PH_X,   FREQ_PH_S, &
     &          FREQ_RATE_X, FREQ_RATE_S, &
     &          AUTO_SUP, USER_SUP, USER_REC, DTEC_FLG, &
     &          IDATYP, OPP_STATUS, PAMB_STATUS, TAU_OC, RATE_OC, TAU_E, RATE_E )
           IF ( ILAST_OBORG_I2 == 7777 .AND. BTEST ( DTEC_FLG, DTH__STS ) ) IDATYP = IDATYP_SAVE 
           IF ( ILAST_OBORG_I2 == 7777 .AND. BTEST ( DTEC_FLG, DTL__STS ) ) IDATYP = IDATYP_SAVE 
!
! -------- Restoration of modified variables. They appeared to be calibrated.
!
           DT = DT_SAVE  + COR_TAU*1.D6
           RT = RT_SAVE  + COR_RATE
!
           IF ( INDEX ( SOLVE_DEBUG, 'IONO_TEST' ) > 0  .AND. &
     &          FL_VTD_IONO                             .AND. &
     &          L_TCN > 0                                     ) THEN
                CALL IONO_TEST_NOI ( L_TCN, TCN, STA_NAM, IONO_ZEN_AVR, &
     &                               DEL_GPS_IONO, FREQ_GR_X, ELEV, &
     &                               RAN_SEED_I4, 100.0D0, TAU_OC )
           END IF
!
           IF ( INDEX ( SOLVE_DEBUG, 'TROPO_TEST' ) > 0 ) THEN
                CALL TROPO_TEST_NOI ( TROP_WZD, AP, 1.0D0, RAN_SEED_I4, &
     &                                TAU_OC )
           END IF
!
! -------- Test of the order of the observations
!
           IF ( WAS_FIRST ) THEN
                TT = (FJD - FJD_1) + (FRACTC - FRACTC_1)
                IF ( (TT_LAST - TT)*86400.0D0 .GT. EPS_SEC ) THEN
                     CALL CLRCH ( STR  )
                     CALL INCH  ( KOBS, STR )
                     CALL CLRCH ( STR1 )
                     WRITE ( UNIT=STR1, FMT='(F20.6)' ) (TT_LAST - TT)*86400.D0
                     CALL CHASHL  ( STR1 )
                     STR2 = MJDSEC_TO_DATE ( MJD_UTC_OBS, UTC_OBS, -2 )
                     CALL ERR_LOG ( 8365, IUER, 'LOOP', 'Wrong '// &
     &                   'order of observations detected in the session '// &
     &                    B3DOBJ%DBNAME_MES//' : '//STR(1:I_LEN(STR))// &
     &                   '-th observation at scan '//SCAN_NAME// &
     &                   ' occured BEFORE the previous '// &
     &                   'one at '//STR1(1:I_LEN(STR1))// &
     &                   ' sec. Time tag: '//STR2(1:22)// &
     &                   ' Database file should be cured!!!' )
                     RETURN
                 END IF
!
                 IF ( DABS(TT_LAST - TT)*86400.0D0 .GT. EPS_SEC  .OR. &
     &                ISTAR_LAST .NE. ISTAR ) THEN
!
                      NUMSCA_NEW = NUMSCA_NEW + 1
                 END IF
                 TT_LAST    = TT
                 ISTAR_LAST = ISTAR
              ELSE
!
! -------------- It is the first analyzed observation
!
                 FJD_1      = FJD
                 FRACTC_1   = FRACTC
                 TT_LAST    = 0.D0
                 ISTAR_LAST = -1
                 WAS_FIRST  = .TRUE.
                 NUMSCA_NEW = 1
           END IF
!
           IF ( ( DATYP_INQ ( IDATYP, XBAND__DTP ) .OR. &
     &            DATYP_INQ ( IDATYP, COMB__DTP  ) .OR. &
     &            FL_USED_AS_X                     .OR. &
     &            FL_USED_AS_XS                         ) .AND. &
     &          BAD_OBS ( LQUAL_CHR )                           ) THEN
!
                TAU_OC = 0.0D0
                TAU_E  = 0.0D0
                TAUGR_OBS_X = 0.0D0
                RATE_OBS_X  = 0.0D0
                TAUGR_ERR_X = 0.0D0
                USER_SUP = IBSET ( USER_SUP, INT4(IDATYP) )
           END IF
!
           IF ( ( DATYP_INQ ( IDATYP, SBAND__DTP  ) .OR. &
     &            DATYP_INQ ( IDATYP, COMB__DTP   ) .OR. &
     &            FL_USED_AS_XS                          ) .AND. &
     &            .NOT. DATYP_INQ ( IDATYP, IOCAL__DTP   ) .AND. &
     &            BAD_OBS ( LQUALXS_CHR )                        ) THEN
!
                TAU_OC = 0.0D0
                TAU_E  = 0.0D0
                TAUGR_OBS_S = 0.0D0
                RATE_OBS_S  = 0.0D0
                TAUGR_ERR_S = 0.0D0
                USER_SUP = IBSET ( USER_SUP, INT4(IDATYP) )
           END IF
           IF ( FL_ADDW ) THEN
                IF ( ADDW_USE == ADDW__REPL ) THEN
                     TAU_E = ADDW_EXT(NOBS)
                END IF
           END IF
           IF ( FL_EXT_ERR ) THEN
                TAU_E = EXT_ERR(NOBS)
                IF ( TAU_E > TAU_E_MAX ) THEN
                     USER_SUP = IBSET ( USER_SUP, INT4(IDATYP) )
                END IF
           END IF
!
           IF ( ELEV(1) < ELEV_DWNT .OR. ELEV(2) < ELEV_DWNT ) THEN
!
! ------------- Elevation angle downweighting
!
                TAU_E = TAU_E * DWNT_FACTOR
                WRITE ( 6, 216 ) 'Elevation', NOBS,   &
      &                 ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
      &                 ISTRN_CHR(ISTAR) 
 216            FORMAT ( 'LOOP: ', A, ' downweight obs ', I6, ' Sta: ', A, 1X, A, &
     &                    ' Sou: ', A )
           END IF 
           IF ( (FJD+FRACTC - TMIN)*86400.0D0 - EPS_SEC < TIM_UPWEI_BEG .OR. &
     &          (FJD+FRACTC - TMIN)*86400.0D0 + EPS_SEC > TIM_UPWEI_END      ) THEN
                TAU_E = TAU_E * DWNT_FACTOR
                WRITE ( 6, 216 ) 'Time segment', NOBS,   &
      &                 ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
      &                 ISTRN_CHR(ISTAR) 
           END IF
!
! -------- Setting flags of suppression status
!
           IF ( SUPMET == SUPMET__META ) THEN
                CALL AUTO_SUP_UPD ( ISITE, ISTAR, ELEV, AUTO_SUP )
              ELSE
                CALL SUPSTAT_SET ( IUNW, IUNWP, LQUAL, LQUALXS, &
     &                       ICORR, GIONSG, PHIONS, IWVBIT1, ISITE, &
     &                       JSITI, ITT, ISTAR, ELEV, KIONO, SNR, SNR_S, &
     &                       SUPSTAT, UACSUP )
           END IF
!
           IF ( FL_DTEC .AND. SUPMET == SUPMET__META .AND. IDATYP == G_GXS__DTP ) THEN
!
! ------------- Get usage flags for XS, X, and S band solutions
!
                FL_USED_GXS = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                IDATYP_SAVE = IDATYP
                IDATYP = GX__DTP
                FL_USED_GX  = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                IDATYP = GS__DTP
                FL_USED_GS  = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                IDATYP = IDATYP_SAVE 
                IONO_FX = -FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
                IONO_FS = -FREQ_GR_X**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
!
                IF ( FL_USED_GXS ) THEN
                     CONTINUE 
                     IF ( SOLVE_DEBUG == '48' ) THEN
                          TAU_IGX   = ( TAUGR_OBS_X - TAUGR_OBS_S )*IONO_FX
                          TAU_IGS   = ( TAUGR_OBS_X - TAUGR_OBS_S )*IONO_FS
                          WRITE ( LUN_DEBUG, 222 ) NOBS, MJD_TAI_OBS, TAI_OBS, &
     &                            ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                            'X', 1.D12*TAU_IGX, &
     &                            1.D12*(DTEC_GPS_EXT(2,NOBS) - DTEC_GPS_EXT(1,NOBS))*VIO__CONST/FREQ_GR_X**2, &
     &                            1.D12*(DTEC_ADJ_EXT(2,NOBS) - DTEC_ADJ_EXT(2,NOBS))*VIO__CONST/FREQ_GR_X**2, &
     &                            1.D12*DEL_BIAS_UL_EXT(NOBS), &
     &                            1.D12*( TAU_IGX - DEL_BIAS_UL_EXT(NOBS) - &
     &                                    ( (DTEC_ADJ_EXT(2,NOBS) - DTEC_ADJ_EXT(1,NOBS)) + &
     &                                      (DTEC_GPS_EXT(2,NOBS) - DTEC_GPS_EXT(1,NOBS)) )*VIO__CONST/FREQ_GR_X**2 &
     &                                  ), &
     &                            1.D12*TAU_E, &
     &                            1.D12*TAU_IGS, &
     &                            1.D12*( TAU_IGS - FREQ_GR_X**2/FREQ_GR_S**2 * DEL_BIAS_UL_EXT(NOBS) - &
     &                                    ( (DTEC_ADJ_EXT(2,NOBS) - DTEC_ADJ_EXT(1,NOBS)) + &
     &                                      (DTEC_GPS_EXT(2,NOBS) - DTEC_GPS_EXT(1,NOBS)) )*VIO__CONST/FREQ_GR_S**2 &
     &                                  )
 222                      FORMAT ( 'DUAL nobs: ', i6, ' MJD: ', I5, ' TAI = ', F7.1, &
     &                             ' bas: ', A, 1X, A, ' Band: ', A, ' tau_igx= ', f12.2, &
     &                             ' tau_dtec_gps= ', F12.2, ' tau_dtec_adj= ', F12.2, &
     &                             ' tau_bias= ', F12.2, ' tau_res= ', F12.2, ' tau_err= ', F8.2, ' ps || ', &
     &                             ' tau_igs= ', F12.2, ' tau_res= ', F12.2, ' ps' )
                     END IF
                END IF
           END IF
!
! -------- If elevation cutoff is negative, then downweight low elevation
! -------- observations
!
           IF ( ELEV(1) < -ELVCUT(ISITE(1)) ) THEN
                TAU_E = DSQRT ( TAU_E**2 + 2.D-8**2 )
           END IF
           IF ( ELEV(2) < -ELVCUT(ISITE(2)) ) THEN
                TAU_E = DSQRT ( TAU_E**2 + 2.D-8**2 )
           END IF
!
! -------- pre1996, archaic decimation ?
!
           IF ( EVINT4 .GT. 1  .AND.  EVINT4 .LE. MAX_OBS ) THEN
                IF ( EVSTART4 .NE. MOD(OBCOUNTR,EVINT4) ) THEN
!
! ------------------ Increment counter of skipped records in order to keep
! ------------------ position in user partial file and go to the next
! ------------------ observation
!
                     SKIP_COUNT = SKIP_COUNT+1
                     GOTO 1000
                END IF
           END IF
           IF ( EDC_USE == EDC__REQ .OR. &
     &          EDC_USE == EDC__USE      ) THEN
!
! ------------- Post OCT2007 decimation. Set decimation bits in suppression
! ------------- bit field
!
                IF ( EDC_USE == EDC__REQ ) CALL ERR_PASS ( IUER, IER )
                IF ( EDC_USE == EDC__USE ) CALL ERR_PASS ( 0,    IER )
                CALL ERR_PASS ( IUER, IER )
                CALL EDC_SET  ( EDC, EDC_PAR, NOBS, ISITE, ISTAR, &
     &                          MJD_TAI_OBS, TAI_OBS, SUPSTAT, AUTO_SUP, IER )
                IF ( IER .NE. 0  .AND. EDC_USE == EDC__REQ ) THEN
                     CALL ERR_LOG ( 8366, IUER, 'LOOP', 'Error in EDC_SET' )
                     RETURN
                END IF
             ELSE IF ( EDC_USE == EDC__CRE ) THEN
                CALL ERR_PASS ( IUER, IER )
                IF ( SUPMET == SUPMET__META ) THEN
                     FL_USED = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                         USED__SPS )
                   ELSE
                     FL_USED = SUPR_INQ ( SUPSTAT, UACSUP, USED__SPS )
                END IF
!
                CALL EDC_UPDATE ( EDC, NOBS, ISITE, ISTAR, MJD_TAI_OBS, &
     &                            TAI_OBS, .NOT. FL_USED, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8367, IUER, 'LOOP', 'Error in EDC_UPDATE' )
                     RETURN
                END IF
           END IF
!
! -------- Update of statistics of the database for the NOBS-th observation
!
           CALL ERR_PASS   ( IUER, IER )
           CALL SESTAT_OBS ( DBOBJ, ISTAR, ISITE, SUPMET, SUPSTAT, UACSUP, &
     &                       AUTO_SUP, USER_SUP, USER_REC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( NOBS, STR )
                CALL ERR_LOG ( 8368, IUER, 'LOOP', 'Error during '// &
     &              'attempt to update data structures for collecting '// &
     &              'statistics of the database '//B3DOBJ%DBNAME_MES// &
     &              ' while the '//STR(1:I_LEN(STR))//'-th observation was '// &
     &              'processed' )
                RETURN
           END IF
           OBCOUNTR = OBCOUNTR + 1
!
! -------- Suppressed?
!
           IF ( SUPMET == SUPMET__META ) THEN
                FL_USED = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                    USED__SPS )
                FL_GOOD = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, GOOD__SPS )
                FL_RECO = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, RECO__SPS )
                IF ( FL_USED_AS_XS ) THEN
                     IDATYP_SAVE = IDATYP
                     IDATYP      = G_GXS__DTP
                     FL_USED = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                     IF ( TAU_E  == 0.0D0 ) FL_USED = .FALSE.
                     IDATYP = IDATYP_SAVE
                END IF
                IF ( FL_USED_AS_X ) THEN
                     IDATYP_SAVE = IDATYP
                     IDATYP      = GX__DTP
                     FL_USED = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                     IF ( TAU_E  == 0.0D0 ) FL_USED = .FALSE.
                     IDATYP = IDATYP_SAVE
                END IF
              ELSE
                FL_USED = SUPR_INQ ( SUPSTAT, UACSUP, USED__SPS )
           END IF
!
           IF ( FL_AOC ) THEN
!
! ------------- Apply a priori observation correction
!
                TAU_OC = TAU_OC + AOC_DEL(NOBS)
#ifdef DEBUG
                WRITE ( 6, 245 ) NOBS, 1.D12*( DER_DEL(VTD__IONO2) - DER_DEL(VTD__IONO1) + AOC_DEL(NOBS)), &
     &                                 1.D12*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)*(TAUGR_OBS_S - TAUGR_OBS_X), &
     &                                 1.D12*AOC_DEL(NOBS), FL_USED
 245            FORMAT ( 'NOBS= ', I5, ' Del1= ', F12.2, ' Del2= ', F12.2, ' ps  Del3= ', F9.2, ' Flag: ', L1 ) 
#endif
!
                IF ( FL_USED ) THEN
                     IONO_VLBI = -(TAUGR_OBS_X - TAUGR_OBS_S)*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
#ifdef DEBUG
                     WRITE  ( 6, 249 ) nobs, 1.D12* ( AOC_DEL(NOBS) + IONO_VLBI ), DBNAME_CH
 249                 format ( 'III-III nobs= ', i5, ' dd= ', f12.2, ' dbname: ', A )
#endif
                END IF
           END IF
!          write ( 6, * ) ' PROC(loop)-2330  NOBS= ', NOBS ; call flush ( 6 ) ! %%%%%
!
! -------- Checking a kludge variable SOLVE_SNR_MIN. If it set, then
! -------- observations with low SNR are deselected
!
           IF ( ( DATYP_INQ ( IDATYP, XBAND__DTP ) .OR. &
     &            DATYP_INQ ( IDATYP, COMB__DTP  ) .OR. &
     &            FL_USED_AS_X                     .OR. &
     &            FL_USED_AS_XS                         ) .AND. &
     &          SNR < SOLVE_SNR_MIN                     ) THEN
                FL_USED = .FALSE.
           END IF
           IF ( ( DATYP_INQ ( IDATYP, SBAND__DTP ) .OR. &
     &            DATYP_INQ ( IDATYP, COMB__DTP  ) .OR. &
     &            FL_USED_AS_XS                         ) .AND. &
     &          SNR_S < SOLVE_SNR_MIN                           ) THEN
                FL_USED = .FALSE.
           END IF
!          write ( 6, * ) ' PROC(loop)-2348  NOBS= ', NOBS ; call flush ( 6 ) ! %%%%%
           IF ( SOLVE_DEBUG(1:1) == '5' ) THEN
                STR = MJDSEC_TO_DATE ( MJD_UTC_OBS, UTC_OBS, -2 )
                WRITE ( LUN_DEBUG, 232 ) NOBS, SCAN_NAME, STR(1:23), &
     &                 ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                 ISTRN_CHR(ISTAR), FL_USED, RATE_OBS_X, RATE_CALC, &
     &                 AZ(1)/DEG__TO__RAD
 232            FORMAT ( I5, 1X, A, 1X, A, 1X, A8,'/',A8, 2X, A, ' || ', &
     &                   ' Used: ', L1, &
     &                   ' Rat_obs: ', 1PD19.12, ' Rat_calc: ', 1PD19.12, &
     &                   ' AZ_1: ', 0PF7.2 )
             ELSE IF ( SOLVE_DEBUG(1:2) == '66' ) THEN
                STR = MJDSEC_TO_DATE ( MJD_UTC_OBS, UTC_OBS, -2 )
!@                IF ( DER_DEL(VTD__COR_FD1) > CORAMP_CUTOFF ) THEN
!@                     TAU_E = DSQRT ( (TAU_E**2 - TAUGR_ERR_X**2) + &
!@     &                               (TAUGR_ERR_X/DER_DEL(VTD__COR_FD1))**2 )
!@                     CONTINUE 
!@                   ELSE IF ( DER_DEL(VTD__STRUC) .NE. 0.0D0 ) THEN
!@                     TAU_E = 1.D-8
!@                     TAU_OC = TAU_OC + DER_DEL(VTD__STRUC)
!@                END IF
                IF ( DER_DEL(VTD__STRUC) .NE. 0.0D0 ) THEN
                     LAMBDA  = VTD__C/FREQ_GR_X
                     UV_PROC = DSQRT ( DER_DEL(VTD__UVX)**2 + DER_DEL(VTD__UVY)**2 )/LAMBDA
                     WRITE ( LUN_DEBUG, 237 ) NOBS, SCAN_NAME, STR(1:23), &
     &                      ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                      ISTRN_CHR(ISTAR), FL_USED, 1.D12*DER_DEL(VTD__STRUC), &
     &                      1.D12*TAU_E, DER_DEL(VTD__COR_FD1), 1.D-6*UV_PROC
 237                 FORMAT ( I5, 1X, A, 1X, A, 1X, A8,'/',A8, 2X, A, ' || ', &
     &                        ' Used: ', L1, ' Del_str: ', F9.2, &
     &                        ' ps  Err_del: ', F9.2, ' ps  Cor_amp: ', F9.5, &
     &                        ' UV_proj: ', F7.2, ' Mlambda' )
                END IF
             ELSE IF ( SOLVE_DEBUG(1:1) == '6' ) THEN
                STR = MJDSEC_TO_DATE ( MJD_UTC_OBS, UTC_OBS, -2 )
                WRITE ( 16, 234 ) NOBS, SCAN_NAME, STR(1:23), &
     &                 ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                 ISTRN_CHR(ISTAR), &
     &                 DER_DEL(27)*1.D9, DER_DEL(28), DER_DEL(29)*1.D9, &
     &                 DER_DEL(30)*1.D9, DER_DEL(31), DER_DEL(32)*1.D9
 234            FORMAT ( I5, 1X, A, 1X, A, 1X, A8,'/',A8, 2X, A, ' || ', &
     &                   3(F10.6,1X), ' || ', 3(F10.6,1X) )
                WRITE ( LUN_DEBUG, 239 ) NOBS, SCAN_NAME, STR(1:23), &
     &                 ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                 ISTRN_CHR(ISTAR), NUMAMB, FAMB, FL_USED 
 239            FORMAT ( I5, 1X, A, 1X, A, 1X, A8,'/',A8, 2X, A, ' || ', &
     &                   ' Num_amb: ', I6, ' FAMB= ', 1PD12.6, ' Used: ', L1 )
             ELSE IF ( SOLVE_DEBUG(1:1) == '7' ) THEN
!
! ------------- Printing intermediate quanitities in the debugging mode
!
                STR = MJDSEC_TO_DATE ( MJD_UTC_OBS, UTC_OBS, -2 )
                WRITE ( UNIT=LUN_DEBUG, FMT=260 ) NOBS, SCAN_NAME, STR(1:23), &
     &                  ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                  ISTRN_CHR(ISTAR), LQUAL_CHR, LQUALXS_CHR, &
     &                  TAUGR_OBS_X, TAUGR_OBS_S, RATE_OBS_X, RATE_OBS_S, &
     &                  TOTPH*DEG__TO__RAD, TOTPH_S*DEG__TO__RAD, &
     &                  SNR, SNR_S, TAUGR_ERR_X, TAUGR_ERR_S, FL_USED
 260            FORMAT ( I5, 1X, A, 1X, A, 1X, A8,'/',A8, 1X, A, 1X, A2, &
     &                   '/', A2, 1X, 1PD21.14, 1X, 1PD21.14, 1X, &
     &                   1PD21.14, 1X, 1PD21.14, 1X, 0PF8.5, &
     &                   1X, F8.5, 1X, F8.3, 1X, F8.3, 1X, 1PD10.3, 1X, &
     &                   1PD10.3, 1X, L1 )
!@             ELSE IF ( FL_USED  .AND. &
!@     &                 FREQ_GR_S > 2.0D9 .AND. FREQ_GR_S < 2.5D9 .AND. &
!@     &                 FREQ_GR_X > 8.0D9 .AND. FREQ_GR_X < 8.9D9 .AND. &
!@     &                 SOLVE_DEBUG(1:8) == 'GET_IONO'                  ) THEN
             ELSE IF ( FL_USED  .AND. &
     &                 FREQ_GR_S > 4.0D9 .AND. FREQ_GR_S < 4.5D9 .AND. &
     &                 FREQ_GR_X > 7.2D9 .AND. FREQ_GR_X < 7.9D9 .AND. &
     &                 SOLVE_DEBUG(1:8) == 'GET_IONO'                  ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL GET_IONO_AVR_COV ( %VAL(VTD_ADR), &
     &                   ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                   IONO_ZEN_AVR, IONO_ZEN_COV, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8369, IUER, 'LOOP', 'Error in an '// &
     &                   'attempt to extract ionosphere path delay '// &
     &                   'statistics' )
                     RETURN
                END IF
                BAS_LEN = DSQRT ( (VSITEC(1,ISITE(1)) - VSITEC(1,ISITE(2)))**2 &
     &                           +(VSITEC(2,ISITE(1)) - VSITEC(2,ISITE(2)))**2 &
     &                           +(VSITEC(3,ISITE(1)) - VSITEC(3,ISITE(2)))**2 )
                CALL REF_ELL ( 0, VSITEC(1:3,ISITE(1)), LAT_GCN(1), LAT_GDT(1), &
     &                         LONG(1), VAL_R8, VAL_R8, VAL_R8 )
                CALL REF_ELL ( 0, VSITEC(1:3,ISITE(2)), LAT_GCN(2), LAT_GDT(2), &
     &                         LONG(2), VAL_R8, VAL_R8, VAL_R8 )
                WRITE ( LUN_DEBUG, 280 ) DBNAME_CH, ISITN_CHR(ISITE(1)), &
     &                           ISITN_CHR(ISITE(2)), BAS_LEN, &
     &                           (FJD + FRACTC - FJDOBS)*86400.0D0, &
     &                           -(TAUGR_OBS_X - TAUGR_OBS_S)* &
     &                           FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2), &
     &                           DER_DEL(VTD__IONO1), DER_DEL(VTD__IONO2), &
     &                           DER_DEL(VTD__DER_IONO1), DER_DEL(VTD__DER_IONO2), &
     &                           TAUGR_ERR_X, IONO_ZEN_AVR, IONO_ZEN_COV, &
     &                           DSQRT(TAUGR_ERR_X**2 + TAUGR_ERR_S**2)* &
     &                           FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2), &
     &                           DER_DEL(VTD__IONO2) - DER_DEL(VTD__IONO1), &
     &                           ELEV(1)/DEG__TO__RAD, ELEV(2)/DEG__TO__RAD, &
     &                           AZ(1)/DEG__TO__RAD, AZ(2)/DEG__TO__RAD, &
     &                           LONG(1)/DEG__TO__RAD, LAT_GDT(1)/DEG__TO__RAD, &
     &                           DER_DEL(VTD__SUR_PWP1)/DEG__TO__RAD, &
     &                           DER_DEL(VTD__SUR_PRS1)/DEG__TO__RAD, &
     &                           LONG(2)/DEG__TO__RAD, LAT_GDT(2)/DEG__TO__RAD, &
     &                           DER_DEL(VTD__SUR_PWP2)/DEG__TO__RAD, &
     &                           DER_DEL(VTD__SUR_PRS2)/DEG__TO__RAD
 280            FORMAT ( A, 2X, A, ' / ', A, 2X, F9.0, 2X, F9.2, 2X, &
     &                   1PD14.6, 2X, 1PD14.6, 2X, 1PD14.6, 2X, &
     &                   0PF5.3, 1X, 0PF5.3, 2X, 1PD11.3, &
     &                   ' A ', 1PD11.3, 1X, 1PD11.3, ' C ', &
     &                   3(1PD11.3, 1X), ' E: ', 1PD11.3, &
     &                   ' Del: ', 1PD11.3, ' Elev: ', 0PF6.2, 1X, F6.2, &
     &                   ' Azim: ', F7.2, 1X, F7.2, &
     &                   ' Long_1/Lat_1: ', F8.2, 1X, F8.2, &
     &                   ' PP_Long_1/Lat_1: ', F8.2, 1X, F8.2, &
     &                   ' Long_2/Lat_2: ', F8.2, 1X, F8.2, &
     &                   ' PP_Long_2/Lat_2: ', F8.2, 1X, F8.2 )
             ELSE IF ( GIM_COLLECT_INFO .OR. SOLVE_DEBUG(1:8) == 'PUT_IONO' ) THEN
                IDATYP_SAVE = IDATYP
                IDATYP      = G_GXS__DTP
                FL_USED_GXS = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                        USED__SPS )
                IDATYP      = GX__DTP
                FL_USED_GX = META_SUPR_INQ  ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                        USED__SPS )
                IDATYP      = GS__DTP
                FL_USED_GS = META_SUPR_INQ  ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                        USED__SPS )
                IDATYP = IDATYP_SAVE 
                STR = MJDSEC_TO_DATE ( MJD_UTC_OBS, UTC_OBS, -2 )
                IF ( IS_R8_NAN(TAUGR_OBS_S) ) TAUGR_OBS_S = 0.0D0
!
                IF ( DER_DEL(VTD__DER_IONO1) == 0.0D0 ) DER_DEL(VTD__DER_IONO1) = 1.0D0
                IF ( DER_DEL(VTD__DER_IONO2) == 0.0D0 ) DER_DEL(VTD__DER_IONO2) = 1.0D0
!@                IF ( DER_DEL(VTD__DER_IONO1) == 0.0D0 .OR. &
!@     &               DER_DEL(VTD__DER_IONO2) == 0.0D0      ) THEN
!@                     CALL CLRCH ( STR )
!@                     CALL INCH  ( NOBS, STR )
!@                     CALL ERR_LOG ( 8370, IUER, 'LOOP', 'Trap of internal control: '// &
!@     &                   'no ionospheric contribution is computed accoring to the VTD '// &
!@     &                   'control file, but collecting ionospheric information is '// &
!@     &                   'requested for observations '//TRIM(STR)//' at baseline '// &
!@     &                   ISITN_CHR(1)//'/'//ISITN_CHR(2) )
!@                     RETURN
!@                END IF 
                IF ( VTD_IONO_SCALE < 1.D-6 ) THEN
                     CALL ERR_LOG ( 8371, IUER, 'LOOP', 'Trap of internal control: '// &
     &                   'IONOSPHERE_SCALE defined in the VTD file '//TRIM(VTD_CONF_USE)// &
     &                   ' is zero. Ionospheric information cannot be collected in this case' )
                     RETURN
                END IF
!
                WRITE ( LUN_DEBUG, 290 ) DBNAME_CH, NOBS, &
     &                  FL_USED_GX, FL_USED_GS,  FL_USED_GXS, &
     &                  ISTRN_CHR(ISTAR), &
     &                  ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                  IDNINT(FJD - 2400000.5D0), FRACT*86400.0D0 - UTC_M_TAI, &
     &                  TAUGR_OBS_X, TAUGR_OBS_S, &
     &                  TAUGR_ERR_X, TAUGR_ERR_S, &
     &                  FREQ_GR_X, FREQ_GR_S, &
     &                  DER_DEL(VTD__IONO1)/DER_DEL(VTD__DER_IONO1)/VTD_IONO_SCALE, &
     &                  DER_DEL(VTD__IONO2)/DER_DEL(VTD__DER_IONO2)/VTD_IONO_SCALE, &
     &                  DER_DEL(VTD__DER_IONO1), DER_DEL(VTD__DER_IONO2), &
     &                  STR(1:24), ELEV(1)/DEG__TO__RAD, AZ(1)/DEG__TO__RAD, &
     &                             ELEV(2)/DEG__TO__RAD, AZ(2)/DEG__TO__RAD, utc_m_tai, utc_obs, 86400.0*fract
 290            FORMAT ( 'PUT_IONO: ', A, ' Ind_Obs: ', I6, ' Fl_used_XSd: ', &
     &                   L1, 1X, L1, 1X, L1, &
     &                   2X, A, 2X, A, ' / ', A, 2X, &
     &                   ' MJD= ', I5, ' TAI= ', 0PF9.3, 2X, &
     &                   ' Tau_obs_XS= ', 1PD20.12, 2X, 1PD20.12, &
     &                   ' Tau_err_XS= ', 1PD14.6, 2X, 1PD14.6, &
     &                   ' Frq= ', 1PD14.6, 2X, 1PD14.6, &
     &                   ' Iono_zen= ', 1PD14.6, 2X, 1PD14.6, &
     &                   ' Iono_map= ', 0PF5.3, 1X, 0PF5.3, &
     &                   ' Date_utc= ', A, ' ', &
     &                   ' El_1= ', F8.4, ' Az_1= ', F8.4, &
     &                   ' El_2= ', F8.4, ' Az_2= ', F8.4, ' || ', f12.2, ' ', f12.2, ' fract: ', f12.2 ) ! %%%%
           END IF
           IF ( GIM_EST .OR. GIM_RGR ) THEN
!
! ------------- Do not do further computation if we are in the ionosphere 
! ------------- bias estimationmode
!
                GOTO 1000 
           END IF
           IF ( SOLVE_DEBUG(2:2) == '1' ) THEN
                IF ( SUPMET == SUPMET__META ) THEN
                     FL_RECO = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                         RECO__SPS )
                   ELSE
                     FL_RECO = SUPR_INQ ( SUPSTAT, UACSUP, RECO__SPS )
                END IF
                WRITE ( UNIT=LUN_DEBUG, FMT=261 ) NOBS, SUPSTAT, AUTO_SUP, USER_SUP, &
     &                                            USER_REC, FL_USED, FL_RECO, &
     &                                            SNR, SNR_S
 261            FORMAT ( 'Obs: ',I5,' Supstat: ', B16, 1X, B16, &
     &                   ' AUTO_SUP: ', B32, ' USER_SUP: ', B32, &
     &                   ' USER_REC: ', B32, ' USED: ', L1, ' RECO: ', L1, &
     &                   ' SNR_X: ', F7.1, ' SNR_S: ', F7.1 )
              ELSE IF ( SOLVE_DEBUG(2:2) == '3' ) THEN
                WRITE ( UNIT=LUN_DEBUG, FMT=265 ) NOBS, ISTRN_CHR(ISTAR), &
     &                  BTEST ( AUTO_SUP, INT4(DSSO__SPS) )
 265            FORMAT ( 'Obs: ', I5,' Sou: ', A, ' Sel: ', L1 )
              ELSE IF ( SOLVE_DEBUG(1:1) == '3' ) THEN
!
! ------------- Printing intermediate quanitities in the debugging mode
!
                STR = MJDSEC_TO_DATE ( MJD_UTC_OBS, UTC_OBS, -2 )
                WRITE ( UNIT=LUN_DEBUG, FMT=272 ) NOBS, SCAN_NAME, STR(1:23), &
     &                  ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                  ISTRN_CHR(ISTAR), LQUAL_CHR, LQUALXS_CHR, &
     &                  AMPL, AMPL_S, SNR, SNR_S, FL_USED, PIND_OBS, &
     &                  AUTO_SUP, USER_SUP, USER_REC, FREQ_GR_X, FREQ_GR_S
 272            FORMAT ( I5, 1X, A, 1X, A, 1X, A8, '/', A8, 1X, A8, 1X, &
     &                   A2, 1X, A2, 2X, F9.6, 2X, F9.6, 2X, F7.2, &
     &                   1X, F7.2, 2X, L1, 2X, I6, 2X, B32, 1X, B32, 1X, B32, &
     &                   2X, 1PD14.7, 1X, 1PD14.7 )
              ELSE IF ( SOLVE_DEBUG(1:2) == '43' ) THEN
!
! ------------- Printing intermediate quantities in the debugging mode
!
                STR = MJDSEC_TO_DATE ( MJD_UTC_OBS, UTC_OBS, -2 )
                WRITE ( UNIT=LUN_DEBUG, FMT=274 ) NOBS, SCAN_NAME, STR(1:23), &
     &                  ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                  ISTRN_CHR(ISTAR), TAUSB_ERR_X, TAUGR_ERR_X, &
     &                  TAUSB_ERR_X/TAUGR_ERR_X, SNR
 274            FORMAT ( I5, 1X, A, 1X, A, 1X, A8,'/',A8, 1X, A, 1X, &
     &                   'Tausb_err_x: ', 1PD11.3, 1X, &
     &                   'Taugr_err_x: w', 1PD11.3, 1X, &
     &                   'Rat: ', 0PF9.4, ' SNR= ', F8.2 )
              ELSE IF ( SOLVE_DEBUG(1:2) == '44' .AND. &
     &                  FL_USED  .AND. &
     &                  FREQ_GR_S > 2.0D9 .AND. FREQ_GR_S < 2.5D9 .AND. &
     &                  FREQ_GR_X > 8.0D9 .AND. FREQ_GR_X < 8.9D9       ) THEN
!
! ------------- Printing ionosphere related quantities in debugging mode
!
                STR = MJDSEC_TO_DATE ( MJD_TAI_OBS, TAI_OBS, -2 )
                WRITE ( UNIT=LUN_DEBUG, FMT=276 ) NOBS, STR(1:23), &
     &                  ISTRN_CHR(ISTAR), &
     &                  ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                  ELEV(1)/DEG__TO__RAD, ELEV(2)/DEG__TO__RAD, &
     &                  AZ(1)/DEG__TO__RAD, AZ(2)/DEG__TO__RAD, &
     &                  -PI2**2/5.308018D10* &
     &                  (TAUGR_OBS_X - TAUGR_OBS_S)* &
     &                  (FREQ_GR_X**2*FREQ_GR_S**2)/(FREQ_GR_X**2 - FREQ_GR_S**2), &
     &                  PI2**2/5.308018D10*DSQRT(TAUGR_ERR_X**2 + TAUGR_ERR_S**2)* &
     &                  (FREQ_GR_X**2*FREQ_GR_S**2)/(FREQ_GR_X**2 - FREQ_GR_S**2)
 276            FORMAT ( I5, 1X, A, 1X, A, 1X, A8, ' / ', A8, 2X, &
     &                   F5.2, 1X, F5.2, 2X, F7.2, 1X, F7.2, 2X, &
     &                   F8.3, 1X, F8.3 )
              ELSE IF ( SOLVE_DEBUG(1:2) == '45' ) THEN
                STR = MJDSEC_TO_DATE ( MJD_TAI_OBS, TAI_OBS, -2 )
                IDATYP_SAVE = IDATYP
                IDATYP      = G_GXS__DTP
                FL_USED_GXS = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                IDATYP      = GX__DTP
                FL_USED_GX = META_SUPR_INQ  ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                IDATYP      = GS__DTP
                FL_USED_GS = META_SUPR_INQ  ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                IDATYP = IDATYP_SAVE 
                WRITE ( UNIT=LUN_DEBUG, FMT=278 ) NOBS, STR(1:23), &
     &                                            ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                                            FL_USED_GXS, FL_USED_GX, FL_USED_GS, &
                   1.D12*( ( TAUGR_OBS_X*FREQ_GR_X**2 - TAUGR_OBS_S*FREQ_GR_S**2 )/( FREQ_GR_X**2 - FREQ_GR_S**2 ) - (TAU_CALC + COR_TAU)), &
                   1.D12*( TAUGR_OBS_X - (TAU_CALC + COR_TAU) ), &
                   1.D12*( TAUGR_OBS_S - (TAU_CALC + COR_TAU) ), &
     &             SNR, SNR_S, 0.0, 0.0
 278            FORMAT ( 'NOBS: ', I5, 2X, A, 2X, A, ' / ', A, 2X, L1, 1X, L1, 1X, L1, &
     &                   ' OC_xs: ', F12.1, ' OC_x: ', F12.1, ' OC_s: ', F12.1, 2X, &
     &                   ' SNR: ', F6.1, 1X, F6.1, ' IONOV: ', 1PD13.6, 1X, 1PD13.6 )
              ELSE IF ( SOLVE_DEBUG(1:2) == '46' ) THEN
!
! ------------- Printing intermediate quantities in the debugging mode
!
                STR = MJDSEC_TO_DATE ( MJD_UTC_OBS, UTC_OBS, -2 )
                WRITE ( UNIT=LUN_DEBUG, FMT=256 ) NOBS, STR(1:23), &
     &                  ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                  ISTRN_CHR(ISTAR), FL_USED, &
     &                  UV_COOR, SP(1,1), SP(2,1), AP(1,1), AP(2,1), TAU_E
 256            FORMAT ( I5, 1X, A, 1X, A8,'/',A8, 1X, A, 2X, L1, &
     &                   2X, 1PD15.8, 1X, 1PD15.8, 2X, 1PD15.8, 1X, 1PD15.8, &
     &                   2X, 1PD15.8, 1X, 1PD15.8, 2X, 1PD15.8 )
              ELSE IF ( SOLVE_DEBUG(1:2) == '47' ) THEN
!
! ------------- Printing intermediate quantities in the debugging mode
!
                IDATYP_SAVE = IDATYP
                IDATYP      = G_GXS__DTP
                FL_USED_GXS = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                IDATYP      = GX__DTP
                FL_USED_GX = META_SUPR_INQ  ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                IDATYP      = GS__DTP
                FL_USED_GS = META_SUPR_INQ  ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                IDATYP = IDATYP_SAVE 
                STR = MJDSEC_TO_DATE ( MJD_UTC_OBS, UTC_OBS, -2 )
!
! ------------- TAU_O = TAU_OC + TAU_CALC + COR_TAU
!
                WRITE ( UNIT=LUN_DEBUG, FMT=257 ) NOBS, STR(1:23), &
     &                  ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                  ISTRN_CHR(ISTAR), FL_USED_GS, FL_USED_GX, FL_USED_GXS, &
     &                  1.D12*(TAU_OC + TAU_CALC + COR_TAU - TAUGR_OBS_X), &
     &                  1.D12*(TAUGR_OBS_X - TAUGR_OBS_S), FREQ_GR_X, &
     &                  FREQ_GR_S
 257            FORMAT ( I5, 1X, A, 1X, A8,'/',A8, 1X, A, 2X, L1, 1X, L1, 1X, L1, &
     &                   ' Iono_gx= ', F12.2, ' gx-gs= ', F12.2, &
     &                   ' Freq_iono_gx= ', 1PD15.8, ' Freq_iono_gx= ', 1PD15.8 )
              ELSE IF ( SOLVE_DEBUG(1:1) == '8' ) THEN
!
! ------------- Printing intermediate quantities in the debugging mode
!
                STR = MJDSEC_TO_DATE ( MJD_UTC_OBS, UTC_OBS, -2 )
                WRITE ( UNIT=LUN_DEBUG, FMT=270 ) NOBS, SCAN_NAME, STR(1:23), &
     &                  ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                  ISTRN_CHR(ISTAR), &
     &                  AZ(1)/DEG__TO__RAD, AZ(2)/DEG__TO__RAD, &
     &                  ELEV(1)/DEG__TO__RAD, ELEV(2)/DEG__TO__RAD
 270            FORMAT ( I5, 1X, A, 1X, A, 1X, A8,'/',A8, 1X, A, 1X, &
     &                   'Az: ', 1PD15.7, 1X, 1PD15.7, 1X, &
     &                   'El: ', 1PD15.7, 1X, 1PD15.7, 1X  )
             ELSE IF ( SOLVE_DEBUG(1:1) == '9' ) THEN
!
! ------------- Printing intermediate quantities in the debugging mode
!
                STR = MJDSEC_TO_DATE ( MJD_UTC_OBS, UTC_OBS, -2 )
                WRITE ( UNIT=LUN_DEBUG, FMT=370 ) NOBS, SCAN_NAME, STR(1:23), &
     &                  ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                  ISTRN_CHR(ISTAR), &
     &                  TAU_OC, TAU_E, FL_USED, LQUAL_CHR, LQUALXS_CHR, &
     &                  TAU_CALC, TAUGR_OBS_X, COR_TAU
 370            FORMAT ( I5, 1X, A, 1X, A, 1X, A8,'/',A8, 1X, A, 1X, &
     &                   1PD19.12, 1X, 1PD11.4, 1X, L1, ' QC: ', A2, ' / ', A2, &
     &                   ' Tau_calc= ', 1PD19.12, ' Tau_obs= ', 1PD19.12, ' Cor_tau: ', 1PD19.12 )
             ELSE IF ( SOLVE_DEBUG(1:8) == 'OUT_IONO' ) THEN
                IONO_VLBI = -(TAUGR_OBS_X - TAUGR_OBS_S)*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
                WRITE ( UNIT=LUN_DEBUG, FMT=380 ) NOBS, IONO_VLBI, FL_USED
 380            FORMAT ( 'Ind_obs: ', I6, ' IoX_del: ', 1PD13.6, 1X, L1 )
             ELSE IF ( SOLVE_DEBUG(1:8) == 'OUT_ELEV' ) THEN
                WRITE ( UNIT=LUN_DEBUG, FMT=385 ) NOBS, (MJD_TAI_OBS - MJD_1ST)*86400.0D0 + &
     &                                                  (TAI_OBS - TAI_1ST), &
     &                                                  ISITN_CHR(1), ISITN_CHR(2), ELEV, AZ
 385            FORMAT ( 'Nobs: ', I6, ' Tim= ', F8.1, ' Sta= ', A, 1X, A, &
     &                   ' Elev= ', F8.5, 1X, F8.5, ' Az= ', F8.5, 1X, F8.5 )
             ELSE IF ( SOLVE_DEBUG(1:8) == 'DTEC_OUT' ) THEN
                STR = MJDSEC_TO_DATE ( MJD_TAI_OBS, TAI_OBS, -2 )
                FL_USED_GXS = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                IDATYP_SAVE = IDATYP
                IDATYP = GX__DTP
                FL_USED_GX  = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                IDATYP = GS__DTP
                FL_USED_GS  = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                IDATYP = IDATYP_SAVE 
                IONO_FX = -FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
                IONO_FS = -FREQ_GR_X**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
!
                IF ( FL_USED_GXS .AND. FL_USED_GX .AND. FL_USED_GS ) THEN
                     IONO_OBS_X =  ( TAUGR_OBS_X - TAUGR_OBS_S )*IONO_FX + &
     &                              DEL_BIAS_UL*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
                     IONO_APR_X = (TEC_APR(2) - TEC_APR(1))*VIO__CONST/FREQ_GR_X**2
                     IONO_MOD_X = (TEC_APR(2) - TEC_APR(1) + DTEC_ADJ )*VIO__CONST/FREQ_GR_X**2 
                     IONO_RES_X = IONO_OBS_X  - IONO_APR_X
                     IONO_RRS_X = IONO_OBS_X  - IONO_MOD_X
                     IONO_ERR_X = DTEC_ERR*VIO__CONST/FREQ_GR_X**2 
!
                     WRITE ( LUN_DEBUG, 387 ) NOBS, STR(1:21), &
     &                           ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                           1.D12*IONO_OBS_X, 1.D12*IONO_APR_X, 1.D12*IONO_MOD_X, &
     &                           1.D12*IONO_RES_X, 1.D12*IONO_RRS_X, 1.D12*IONO_ERR_X, &
     &                           DER_DEL(VTD__DER_IONO1), DER_DEL(VTD__DER_IONO2), &
     &                           TEC_APR(1), TEC_APR(2), DTEC_ADJ, DEL_BIAS_UL*FREQ_GR_S**2/(FREQ_GR_X**2 - FREQ_GR_S**2)
 387                 FORMAT ( 'Obs: ', I6, ' Tim: ', A, ' Sta: ', A, 1X, A, &
     &                        ' Iono_obs: ', F12.1, ' Iono_apr: ', F12.1, ' Iono_mod: ', F12.1, &
     &                        ' Iono_oc: ', F12.1, ' Iono_res: ', F12.1, ' Iono_err: ', F12.1, &
     &                        '  psec || Iono_map: ', F5.3, 1X, F5.3, &
     &                        ' || Tec_apr: ', F6.1, 1X, F6.1, ' Dtec_adj= ', F6.1, ' Del_bias: ', 1PD13.6, ' sec' )
               END IF
           END IF
           IF ( IONOV_USE == IONOV__GEN .AND. .NOT. FILE_IONO_EX ) THEN
                IDATYP_SAVE = IDATYP
                IDATYP      = G_GXS__DTP
                FL_USED_GXS = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                        USED__SPS )
                IDATYP      = GX__DTP
                FL_USED_GX = META_SUPR_INQ  ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                        USED__SPS )
                IDATYP      = GS__DTP
                FL_USED_GS = META_SUPR_INQ  ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                        USED__SPS )
                IDATYP = IDATYP_SAVE 
                WRITE ( LUN_IONO, 390 ) DBNAME_CH(1:10), NOBS, &
     &                  FL_USED_GX, FL_USED_GS,  FL_USED_GXS, &
     &                  ISTRN_CHR(ISTAR), &
     &                  ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), &
     &                  IDNINT(FJD - 2400000.5D0), FRACT*86400.0D0, &
     &                  TAUGR_OBS_X, TAUGR_OBS_S, &
     &                  TAUGR_OBS_X + RATE_OBS_X*DER_DEL(VTD__DEL_GCN1), &
     &                  TAUGR_OBS_S + RATE_OBS_S*DER_DEL(VTD__DEL_GCN1), &
     &                  TAUGR_X_ERR_ORIG, TAUGR_S_ERR_ORIG, TAU_REWEI, &
     &                  FREQ_GR_X, FREQ_GR_S, &
     &                  DER_DEL(VTD__DER_IONO1), DER_DEL(VTD__DER_IONO2), &
     &                  -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, &
     &                  -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0
 390            FORMAT ( 'IONOV:  ', A,'  Ind_Obs: ', I6, '  Fl_used_XSd: ', &
     &                   3(L1,1X), 1X, A, 2X, A, ' / ',A, &
     &                   '  MJD= ', I5, ' UTC= ', 0PF9.3, 1X, &
     &                   ' Tau_obs= ', 1PD19.12, 2X, 1PD19.12, 1X, &
     &                   ' Tau_gc=  ', 1PD19.12, 2X, 1PD19.12, 1X, &
     &                   ' Tau_err= ', 1PD13.6, 2X, 1PD13.6, 1X, &
     &                   ' TAU_rewei= ', 1PD13.6, 1X, &  
     &                   ' Frq= ', 1PD13.6, 2X, 1PD13.6, 1X, &  
     &                   ' Iono_map= ', 0PF5.3, 1X, 0PF5.3, 1X, &
     &                   ' TAU_ionog= ', 1PD13.6, 2X, 1PD13.6, 1X, &  
     &                   ' TAU_ionov= ', 1PD13.6, 2X, 1PD13.6, 1X, &  
     &                   ' TAU_ionov_err= ', 1PD13.6, 2X, 1PD13.6, 1X, &  
     &                   ' TAU_ionom= ', 1PD13.6, 2X, 1PD13.6, 1X, &  
     &                   ' TAU_ionom_err= ', 1PD13.6, 2X, 1PD13.6, 1X, &  
     &                   ' TAU_ionom_exc= ', 1PD13.6, 2X, 1PD13.6 )
           END IF
!
           IF ( IS_R8_NAN(TAU_OC) ) THEN
                WRITE ( 6, * ) 'Observation ', NOBS, ' -- NaN in TAU_OC'
                CALL EXIT ( 1 )
           END IF
           IF ( IS_R8_INF(TAU_OC) ) THEN
                WRITE ( 6, * ) 'Observation ', NOBS, ' -- Inf in TAU_OC'
                WRITE ( 6, * ) 'TAU= ', TAUGR_OBS_X, TAUGR_OBS_S
                WRITE ( 6, * ) 'FRQ= ', FREQ_GR_X,   FREQ_GR_S
                CALL EXIT ( 1 )
           END IF
           IF ( IS_R8_NAN(TAU_E) ) THEN
                WRITE ( 6, * ) 'Observation ', NOBS, ' -- NaN in TAU_E'
                CALL EXIT ( 1 )
           END IF
           IF ( IS_R8_INF(TAU_E) ) THEN
                WRITE ( 6, * ) 'Observation ', NOBS, ' -- Inf in TAU_E'
                CALL EXIT ( 1 )
           END IF
           IF ( FL_USED .AND. TAU_E == 0.0D0 ) THEN
                WRITE ( 6, * ) 'Observation ', NOBS, ' -- TAU_E = 0.0D0  ', &
     &                         ' QUAL: '// &
     &                         LQUAL_CHR//' / '//LQUALXS_CHR, &
     &                         ' SNR= ', SNGL(SNR), SNGL(SNR_S), &
     &                         ' TAUGR_OBS_X = ', TAUGR_OBS_X, ' SUPMET = ', SUPMET
                LQUAL_CHR = '0 ' 
                CALL EXIT ( 1 )
           END IF
           IF ( INDEX ( SOLVE_EXPORT, 'edit' ) > 0 ) THEN
                IF ( .NOT. FL_USED ) THEN
!
! ------------------ Write a record in the output edit file
!
                     WRITE ( UNIT=LUN_EDIT, FMT='(I6)' ) NOBS
                END IF
           END IF
           IF ( INDEX ( SOLVE_EXPORT, 'addw' ) > 0 ) THEN
!
! ------------- Write a record in the output additive weight file
!
                WRITE ( UNIT=LUN_ADDW, FMT=154 ) NOBS, ADDERR_GR_TAU, DERR_SAVE, TAU_E
 154            FORMAT ( 'Ind_obs: ', I6, ' Add_wei: ', 1PD13.6, &
     &                   ' Orig_wei: ', 1PD13.6, ' Used_wei: ', 1PD13.6 )
           END IF
!
           IF ( INDEX ( SOLVE_EXPORT, 'sca_wei' ) > 0 ) THEN
                STR = MJDSEC_TO_DATE ( MJD_TAI_OBS, TAI_OBS, -2 )
                STR(31:34) = 'FFFF'
                STR(35:35) = '?'
                IF ( IDATYP == GX__DTP     ) STR(35:35) = 'x'
                IF ( IDATYP == GS__DTP     ) STR(35:35) = 's'
                IF ( IDATYP == G_GXS__DTP  ) STR(35:35) = 'd'
                IF ( IDATYP == FUSED__DTP  ) STR(35:35) = 'f'
                IF ( IDATYP == GRPONL__DTP ) STR(35:35) = 'g'
                IF ( SUPMET == SUPMET__META ) THEN
                     IDATYP_SAVE = IDATYP
                     IDATYP = GX__DTP
                     FL_USED_GX  = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                     IF ( FL_USED_GX ) STR(31:31) = 'T'
!
                     IF ( KBIT ( OPP_STATUS, OPP_SET2__BIT ) ) THEN
                          IDATYP = GS__DTP
                          FL_USED_GS = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                          IF ( FL_USED_GS  ) STR(32:32) = 'T'
!
                          IDATYP = G_GXS__DTP
                          FL_USED_GXS  = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                          IF ( FL_USED_GXS ) STR(33:33) = 'T'
                       ELSE
                          STR(32:33) = 'NN'
                     END IF
!
                     IF ( .NOT. FUSED_STATUS ==   IONOV__UNDF ) THEN
                          IDATYP = FUSED__DTP
                          FL_USED_FUSED  = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                          IF ( FL_USED_FUSED ) STR(34:34) = 'T'
                        ELSE
                          STR(34:34) = 'N'
                     END IF
!
                     IDATYP = GRPONL__DTP
                     FL_USED_GRP = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
                     IF ( FL_USED_GRP  ) THEN
                          STR(36:36) = 'T'
                         ELSE 
                          STR(36:36) = 'N'
                     END IF
                     IDATYP = IDATYP_SAVE 
                  ELSE
                     STR(31:34) = 'NNFNN'
                     FL_USED_GXS  = SUPR_INQ ( SUPSTAT, UACSUP, USED__SPS )
                     IF ( FL_USED_GXS ) STR(33:33) = 'T'
                END IF
                WRITE ( UNIT=LUN_SCAW, FMT=388 ) NOBS, STR(1:21), SCAN_NAME, &
     &                  ISITN_CHR(ISITE(1)), ISITN_CHR(ISITE(2)), ISTRN_CHR(ISTAR), &
     &                  STR(35:35), TAU_E, STR(31:31), STR(32:32), STR(33:33), STR(34:34), STR(36:36), &
     &                  DER_DEL(VTD__DER_RA), DER_DEL(VTD__DER_DL)
 388            FORMAT ( 'Ind_obs: ', I6, ' Tim: ', A, ' Sca: ', A, ' Sta: ', A, 1X, A, &
     &                   ' Sou: ', A, ' Err (',A,'): ', 1PD13.6, ' Sup (xsdf): ', 5(A1,1X), &
     &                   ' Der_sou: ', 1PD11.4, 1X, 1PD11.4 )
           END IF
!
           IF ( FL_USED ) THEN
                IF ( FL_WRONG_FREQ_PRINT ) THEN
                     IF ( FREQ_GR_X .GE. 1.D6 .AND. &
     &                    FREQ_GR_S .GE. 1.D6 .AND. &
     &                    .NOT. ( FREQ_GR_X .GE. 8.0D9 .AND. FREQ_GR_X .LE. 9.0D9  .AND. &
     &                            FREQ_GR_S .GE. 2.0D9 .AND. FREQ_GR_S .LE. 2.5D9        )  ) THEN
                          LUN_FREQ = GET_UNIT()
                          OPEN ( UNIT =LUN_FREQ, FILE='/tmp/wrong_freq.txt', STATUS='UNKNOWN', ACCESS='APPEND' )
                                 WRITE ( LUN_FREQ, 144 ) DBNAME_CH, NOBS, FREQ_GR_S, FREQ_GR_X
 144                             FORMAT ( 'Exp: ',A, ' Ind_Obs: ', I7, ' Freqs= ', 1PD13.6, 1X, 1PD13.6 )
                          CLOSE ( UNIT=LUN_FREQ )
                          FL_WRONG_FREQ_PRINT = .FALSE.
                     END IF
               END IF
           END IF
!
           IF ( .NOT. FL_USED ) THEN
!
! ------------- Increment the counter of skipped records in order to keep
! ------------- position in user partial file and go to the next observation
!
                SKIP_COUNT = SKIP_COUNT+1
                GOTO 1000
           END IF
!!           write ( 6, * ) 'loop-2735 nobs= ', nobs ; call flush ( 6 ) ! %%%%%%%%%%
!
! -------- Update the register which keeps time of the last observation
!
           IF ( B3DOBJ%JD_ACT_FIRST .LT. EPS_SEC/86400.D0 ) THEN
                B3DOBJ%JD_ACT_FIRST = FJD + FRACTC
           END IF
           B3DOBJ%JD_ACT_LAST = FJD + FRACTC
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
           DO 470 J7=1,2
              WW_STA(ISITE(J7)) = WW_STA(ISITE(J7)) + 1.D0/TAU_E
              IF ( JDATE_STA_BEG(ISITE(J7)) .LT. -BIG_VALUE/2.0 ) THEN
                   JDATE_STA_BEG(ISITE(J7)) = (FJD + FRACTC)
              END IF
              JDATE_STA_MID(ISITE(J7)) = JDATE_STA_MID(ISITE(J7)) + &
     &                                   (FJD + FRACTC - JDATE_ALL_BEG )/TAU_E
              JDATE_STA_END(ISITE(J7)) = (FJD + FRACTC)
  470      CONTINUE
!
! -------- Calculate the partial derivatives
!
           CALL F__CLR_IND ( 0, FAST_MODE, PLACE, B3DOBJ, B1B3DOBJ )
           CALL PARTL ( DERIV, POLYONLY, PLACE, B3DOBJ )
!
! -------- If delays in the solution, compute appropriate O-C here
!
           IF ( DATYP_INQ ( IDATYP, DELAY__DTP ) )  THEN
!
! ------------- Check the downweight delay flag
!
                IF ( IDNWT.EQ.1 ) TAU_E = 1.0D0
                B3DOBJ%SUWSQ_TAU = B3DOBJ%SUWSQ_TAU + (TAU_OC/TAU_E)**2
           END IF
!
           IF ( DATYP_INQ ( IDATYP, RATE__DTP ) )  THEN
                B3DOBJ%SUWSQ_FRE = B3DOBJ%SUWSQ_FRE + (RATE_OC/RATE_E)**2
           END IF
!
! -------- Increment normal equations for delay measurements
!
           IF ( FAST_MODE .EQ. F__NONE ) THEN
!
!-------------- FULL case. Old, slow variant
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
                IF ( DATYP_INQ ( IDATYP, DELAY__DTP ) ) THEN
!
! ------------------ Include delays in solution
!
                     CALL NRMEQ ( ARR(JA), ARR(JB), NPARAM, DERIV(1,1), &
     &                            TAU_OC, TAU_E, NELEM )
                END IF
                IF ( PLACE%STATUS .EQ. F__RAT ) THEN !
!
! ------------------ Include delay rates in solution
!
                     ROC = ROBS - RT
                     CALL NRMEQ ( ARR(JA), ARR(JB), NPARAM, DERIV(1,2), &
     &                            RATE_OC, RATE_E, NELEM )
                END IF
              ELSE IF ( FAST_MODE .EQ. F__PRD   .OR. &
     &                  FAST_MODE .EQ. F__B1D         ) THEN
!
!-------------- FULL case. Faster, but obsolete variant
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
                IF ( NORMEQ_EXT ) THEN
                     CALL ERR_PASS ( IUER, IUER )
                     CALL EQUOBS_UPD ( PLACE%STATUS .EQ. F__RAT, NOBS, &
     &                                 TAU_OC, TAU_E, ROBS - RT, RERR, &
     &                                 NPARAM, PLACE%N_GEN, PLACE%IND_GEN, &
     &                                 PLACE%EQU_GEN, PLACE%RAT_GEN, EQUOBS, IER  )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH   ( STR )
                          CALL INCH    ( NOBS, STR )
                          CALL ERR_LOG ( 8372, IUER, 'LOOP', 'Error in updating '// &
     &                        'EQUOBS object for including the '//TRIM(STR)// &
     &                        'th observation' )
                          RETURN 
                     END IF
                   ELSE
                     IF ( DATYP_INQ ( IDATYP, DELAY__DTP ) ) THEN
!
! ----------------------- Include delays in solution
!
                          CALL ADD_TRG ( TAU_OC, TAU_E, PLACE%N_GEN, PLACE%IND_GEN, &
     &                         PLACE%EQU_GEN, NPARAM, ARR(JB), ARR(JA) )
                     END IF
                     IF ( PLACE%STATUS .EQ. F__RAT ) THEN
!
! ----------------------- Delay rates
!
                          ROC = ROBS - RT
                          CALL ADD_TRG ( ROC, RERR, PLACE%N_GEN, PLACE%IND_GEN, &
     &                         PLACE%RAT_GEN, NPARAM, ARR(JB), ARR(JA) )
                     END IF
!!        call matview_2 ( nparam, arr(ja) ) ! %%%
                END IF
              ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
!
!-------------- Variant for B3D algorithm
!               ~~~~~~~~~~~~~~~~~~~~~~~~~
!
! ------------- Test: has we reached the next segment boundary?
!
                IF ( F__NEXT_COMSEG ( PLACE, B3DOBJ ) ) THEN

! ------------------ Yes. Updated normal equations using segmented accumulators
!
                     CALL NSG_B3D   ( PLACE%PREV_CSG, PLACE%LAST_CSG, B3DOBJ )
!
! ------------------ Clearing segmented accumulators
!
                     CALL F__CLR_IND  ( 1, FAST_MODE, PLACE, B3DOBJ,B1B3DOBJ )
                END IF
                IF ( DATYP_INQ ( IDATYP, DELAY__DTP ) ) THEN
!
! ------------------ Update segmented accumulators (delay)
!
                     CALL ERR_PASS ( IUER, IER )
!                    write ( 6, * ) 'loop-2878 nobs= ', nobs, ' tai_obs= ', tai_obs, ' bas= ', isitn_chr(isite(1)), ' ', isitn_chr(isite(2)) ; call flush ( 6 ) ! %%%%%%%%%%
                     CALL ADD_B3D  ( F__DEL, TAU_OC, TAU_E, PLACE, B3DOBJ, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( KOBS, STR )
                          CALL ERR_LOG ( 8373, IUER, 'LOOP', &
     &                        'Error during processing '//STR(1:I_LEN(STR))// &
     &                        '-th observation of '//'the database '// &
     &                         B3DOBJ%DBNAME_MES )
                          RETURN
                      END IF
                END IF
                IF ( DATYP_INQ ( IDATYP, RATE__DTP ) ) THEN
!
! ------------------ Update segmented accumulators (rate)
!
                     ROC = ROBS - RT
                     CALL ERR_PASS ( IUER, IER )
                     CALL ADD_B3D  ( F__RAT, RATE_OC, RATE_E, PLACE, B3DOBJ, &
     &                               IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( KOBS, STR )
                          CALL ERR_LOG ( 8374, IUER, 'LOOP', &
     &                        'Error during processing '//STR(1:I_LEN(STR))// &
     &                        '-th observation '//'of the database '// &
     &                         B3DOBJ%DBNAME_MES )
                          RETURN
                     END IF
                END IF
              ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! ------------- Variant for B1B3D algorithm
!               ~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! ------------- Test: has we reached the next segment boundary?
!
                IF ( F__NEXT_COMSEG ( PLACE, B3DOBJ ) ) THEN
!
! ------------------ Yes. Updated normal equations using segmented accumulators
!
                     CALL NSG_B1B3D ( PLACE%PREV_CSG, PLACE%LAST_CSG, B3DOBJ, &
     &                                B1B3DOBJ )
!
! ------------------ Clearing segmented accumulators
!
                     CALL F__CLR_IND ( 1, FAST_MODE, PLACE, B3DOBJ,B1B3DOBJ )
                END IF
                IF ( DATYP_INQ ( IDATYP, DELAY__DTP ) ) THEN
!
! ------------------ Update segmented accumulators
!
                     CALL ERR_PASS  ( IUER, IER )
                     CALL ADD_B1B3D ( F__DEL, TAU_OC, TAU_E, PLACE, B3DOBJ, &
     &                                B1B3DOBJ, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( KOBS, STR )
                          CALL ERR_LOG ( 8375, IUER, 'LOOP', &
     &                        'Error during processing '//STR(1:I_LEN(STR))// &
     &                        '-th observation of '//'the database '// &
     &                         B3DOBJ%DBNAME_MES )
                         RETURN
                     END IF
                END IF
!
                IF ( DATYP_INQ ( IDATYP, RATE__DTP ) ) THEN
!
! ------------------ Update segmented accumulators (rate)
!
                     ROC = ROBS - RT
                     CALL ERR_PASS  ( IUER, IER )
                     CALL ADD_B1B3D ( F__RAT, RATE_OC, RATE_E, PLACE, B3DOBJ, &
     &                                B1B3DOBJ, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL INCH  ( KOBS, STR )
                          CALL ERR_LOG ( 8376, IUER, 'LOOP', &
     &                        'Error during processing '//STR(1:I_LEN(STR))// &
     &                        '-th observation '//'of the database '// &
     &                         B3DOBJ%DBNAME_MES )
                          RETURN
                     END IF
                END IF
            END IF ! Include the observable into solution!
 1000       CONTINUE
            IF ( FL_OBSFIL_UPDATE ) THEN
                 CALL USE_OBSFIL ( IOBSFIL, NOBS, 'W' )
            END IF
          END DO  ! running over the observations!
!
! ======== End of loop
!
          IF ( SOLVE_DEBUG(1:2) == '48' ) THEN
               CLOSE ( LUN_DEBUG )
          END IF
          IF ( INDEX ( SOLVE_EXPORT, 'edit' ) > 0 ) THEN
               CLOSE ( UNIT=LUN_EDIT )
          END IF
          IF ( INDEX ( SOLVE_EXPORT, 'addw' ) > 0 ) THEN
               CLOSE ( UNIT=LUN_ADDW )
          END IF
          IF ( INDEX ( SOLVE_EXPORT, 'scaw' ) > 0 ) THEN
               CLOSE ( UNIT=LUN_SCAW )
          END IF
!
          IF ( SOLVE_DEBUG(1:8) == 'DTEC_OUT' ) THEN
               CLOSE ( UNIT=LUN_DEBUG )
          END IF
!
          IF ( GIM_EST ) THEN
!
! ------------ OK. We are in the ionosphere bias estimation mode.
!
! ------------ Jump out of the program
!
               IF ( GIM_COLLECT_INFO ) THEN
                    CLOSE ( LUN_DEBUG )
               END IF
               IF ( MEM_LEN .NE. 0 ) THEN
                    CALL FREE ( MEM_ADR )
                    MEM_LEN = 0
                    MEM_ADR = 0
               END IF
               CALL ERR_LOG ( 0, IUER )
               RETURN
          END IF
          IF ( GIM_RGR ) THEN
               IF ( .NOT. GIM_EST .AND. GIM_COLLECT_INFO ) THEN
                    CLOSE ( LUN_DEBUG )
               END IF
               IF ( MEM_LEN .NE. 0 ) THEN
                    CALL FREE ( MEM_ADR )
                    MEM_LEN = 0
                    MEM_ADR = 0
               END IF
               CALL ERR_LOG ( 0, IUER )
               RETURN
          END IF
!
          IF ( EDC_USE == EDC__CRE ) THEN
               CALL ERR_PASS ( IUER, IER )
               CALL EDC_WRITE ( EDC, EDC_DIR, EDC_PAR, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 8379, IUER, 'LOOP', 'Error during '// &
     &                  'processing the database '//B3DOBJ%DBNAME_MES// &
     &                  ' -- cannot write the external decimation file' )
                    RETURN
               END IF
          END IF
          IF ( EDC_USE .NE. EDC__UNDF ) THEN
!
! ------------ Release memory grabbed by EDC and initialize EDC
!
               CALL ERR_PASS ( IUER, IER )
               CALL EDC_QUIT ( EDC, IER  )
          END IF
!@   write ( 6, * ) 'loop-2980 ', nobs ; call flush ( 6 ) ! %%%%%%%%%%
!
! ------- Compute weighted mean dates
!
          IF ( WW_ALL .GT. 1.D-20 ) THEN
!
! ------------ over all observations
!
               JDATE_ALL_MID = JDATE_ALL_MID/WW_ALL + JDATE_ALL_BEG
             ELSE
               JDATE_ALL_MID = (JDATE_ALL_END + JDATE_ALL_BEG)/2.0D0
          END IF
!
! ------- ... and over participating stations
!
          DO 480 J8=1,NUMSTA
             IF ( WW_STA(J8) .GT. 1.D0/BIG_VALUE ) THEN
                  JDATE_STA_MID(J8) = JDATE_STA_MID(J8)/WW_STA(J8) + &
     &                                JDATE_ALL_BEG
                ELSE
                  JDATE_STA_MID(J8) = JDATE_ALL_MID
             END IF
  480     CONTINUE
!
! ------- Save them in glbc4
!
          CALL USE_GLBFIL_4 ( 'OWC' )
!
          IF ( NUMSCA_NEW .NE. NUMSCA ) THEN
!
! ------------ Updating NUMSCA in SOCOM area in the case when the new value
! ------------ of NUMSCA differs from the old one.
!
               CALL USE_COMMON ( 'ORC' )
               CALL SOCOM_EXT()
               NUMSCA = NUMSCA_NEW
               CALL USE_COMMON ( 'OWC' )
          END IF
!
          IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! ------------ All observations have been processed. Update blocks of normal
! ------------ matrix using accumulators filled during processing observations
! ------------ of the last segment.
!
               CALL NSG_B3D ( PLACE%LAST_CSG, PLACE%CURR_CSG, B3DOBJ )
            ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! ------------ The same for the case of B1B3D mode
!
               CALL NSG_B1B3D ( PLACE%LAST_CSG, PLACE%CURR_CSG, B3DOBJ, &
     &                                                          B1B3DOBJ )
          END IF
          IF ( KUSER_PART .AND. ( ARC_USER_PART > 0 .OR.  NUM_USER_PART > 0 ) ) THEN
               CALL BIN_CLOSE ( FNAME_PART, FILDES_PART )
          END IF
!
          IF ( KSCREEN ) CALL NL_MN ! new line on screen
!
          B3DOBJ%NOBS_T = KOBS !  Total number of observations
          B3DOBJ%NOBS_A = KOBS - SKIP_COUNT  ! Number of used observations
!
! ------- Update of statistics of the database for the NOBS-th observation
!
!@   write ( 6, * ) 'loop-3046 ', nobs ; call flush ( 6 ) ! %%%%%%%%%%
          CALL ERR_PASS   ( IUER, IER )
          CALL SESTAT_DBS ( DBOBJ, NUMSCA, 0, IER )
          IF ( IER .NE. 0 ) THEN
               CALL CLRCH ( STR )
               CALL INCH  ( NOBS, STR )
               CALL ERR_LOG ( 8380, IUER, 'LOOP', 'Error during '// &
     &             'attempt to update data structures for collecting '// &
     &             'statistics of the database '//B3DOBJ%DBNAME_MES )
               RETURN
          END IF
!
          NPARAM_OLD = NPARAM ! Keep the old value of the number of parameters
!
! ------- Check of deficiency of parameterization which may lead to singularity
! ------- of the normal matrix
!
          IF ( G_WARNING ) THEN
               VERB_SNGCHK = 1
             ELSE
               VERB_SNGCHK = 0
          END IF
!
          SNGCHK_ACTION_SAVED = SNGCHK_ACTION ! Save global value of sngchk
          IF ( SNGCHK_ACTION .EQ. SNGCHK_ACT__REPR  .AND. &
     &         FAST_MODE .EQ. F__NONE               .AND. &
     &         ISLTY2 .NE. 'I'                            ) THEN
!
! ------------ We disable reparameteirzation in global solution of non fast
! ------------ mode.
!
               SNGCHK_ACTION = SNGCHK_ACT__WARN
          END IF
!
!@   write ( 6, * ) 'loop-3080 ', nobs ; call flush ( 6 ) ! %%%%%%%%%%
          CALL ERR_PASS ( IUER, IER )
          CALL SNGCHK   ( DBOBJ, VERB_SNGCHK, R_SOU, RIS_SOU, R_STA, RIS_STA, &
     &                    R_BAS, RIS_BAS, SNGCHK_CMP, IER )
          IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 8381, IUER, 'LOOP', 'Error during '// &
     &             'singularity checking the database '//B3DOBJ%DBNAME_MES )
               RETURN
          END IF
!
          SNGCHK_ACTION = SNGCHK_ACTION_SAVED ! restore
!
! ------- Different actions as a result of singularity tests
!
!@    write ( 6, * ) 'loop-3090 ' , nobs ; call flush ( 6 ) ! %%%%%%%%%%
         IF ( SNGCHK_CMP .EQ. SNGCHK_CMP__CONT ) THEN
               CONTINUE
            ELSE IF ( SNGCHK_CMP .EQ. SNGCHK_CMP__BACK  .AND. &
     &                FAST_MODE .EQ. F__NONE            .AND. &
     &                ISLTY2 .NE. 'I'                         ) THEN
!
! ------------ Remind: reparameterization is not supported for not fast mode
! ------------ in global solution. Reason: there is no way to pass to ARCPE,
! ------------ BACK status of deselected stations/sources/baslines: only
! ------------ normal matrix is saved
!
               WRITE  (  6, 110 ) DBOBJ%NAME(1:I_LEN(DBOBJ%NAME))
               WRITE  ( 23, 110 ) DBOBJ%NAME(1:I_LEN(DBOBJ%NAME))
 110           FORMAT &
     &                  ( '$$$  Request for reparameterization is denied ''in non-fast mode for ',A)
!
! ------------ Now tricky point. We play back and restore the old number of
! ------------ parameters. Why? ARCPE will read socom and it should get that
! ------------ value of parameters which corresponds the parameteriazation
! ------------ which has been actually used in making normal equations.
!
               NPARAM = NPARAM_OLD
               CALL USE_COMMON ( 'OWC' )
            ELSE IF ( SNGCHK_CMP .EQ. SNGCHK_CMP__BACK ) THEN
!
! ------------ Completion code: build the normal system again
!
               IF ( LBACK ) THEN
                    CALL ERR_LOG ( 8382, IUER, 'LOOP', 'Trap of '// &
     &                  'internal control: attempt to make '// &
     &                  'reparameterization on the fly the second '// &
     &                  'consecutive time while the database '// &
     &                   B3DOBJ%DBNAME_MES//' was processed. Please send '// &
     &                  'verbose bug report to Leonid Petrov '// &
     &                  '(pet@leo.gsfc.nasa.gov)' )
                    RETURN
               END IF
               LBACK = .TRUE.
!
! ------------ Transferring lists of sources/stations/baselines participated
! ------------ in solution to B3DOBJ
!
               B3DOBJ%U_SOU = DBOBJ%U_SOU
               CALL LIB$MOVC3 ( 4*MO_SOU, DBOBJ%UIS_SOU, B3DOBJ%UIS_SOU )
               B3DOBJ%U_STA = DBOBJ%U_STA
               CALL LIB$MOVC3 ( 4*MO_STA, DBOBJ%UIS_STA, B3DOBJ%UIS_STA )
               B3DOBJ%U_BAS = DBOBJ%U_BAS
               CALL LIB$MOVC3 ( 4*MO_BAS, DBOBJ%UIS_BAS, B3DOBJ%UIS_BAS )
!
! ------------ Transferring lists of rejected lists to B3DOBJ
!
               B3DOBJ%R_SOU = R_SOU
               CALL LIB$MOVC3 ( 4*MO_SOU, RIS_SOU, B3DOBJ%RIS_SOU )
               B3DOBJ%R_STA = R_STA
               CALL LIB$MOVC3 ( 4*MO_STA, RIS_STA, B3DOBJ%RIS_STA )
               B3DOBJ%R_BAS = R_BAS
               CALL LIB$MOVC3 ( 4*MO_BAS, RIS_BAS, B3DOBJ%RIS_BAS )
!
! ------------ NB! In rary cases NPARAM may increase
!
               IF ( NPARAM .GT. NPARAM_OLD+MAX_ARC_BSL ) THEN
                    WRITE ( 6, * ) ' nparam = ',nparam, ' nparam_old = ', nparam_old
                    CALL ERR_LOG ( 8383, IUER, 'LOOP', &
     &                  'Reparameterization resulted in a substantial '// &
     &                  'increase of the  number of estimated parameters. '// &
     &                  'This case is not supported' )
                    RETURN
               END IF
!
               NELEM = (INT8(NPARAM)*INT8(NPARAM+1))/2
               IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! ----------------- Freeing dynamic memory
!
                    CALL ERR_PASS    ( IUER, IER )
                    CALL B3D_FREEMEM ( B3DOBJ, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 8384, IUER, 'LOOP', &
     &                       'Error during freeing dynamic memory for B3D '// &
     &                       'internal data structures' )
                         RETURN
                    END IF
                  ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! ----------------- Freeing dynamic memory
!
                    CALL ERR_PASS      ( IUER, IER )
                    CALL B1B3D_FREEMEM ( B3DOBJ, B1B3DOBJ, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 8385, IUER, 'LOOP', &
     &                       'Error during freeing dynamic memory for B1B3D '// &
     &                       'internal data structures' )
                         RETURN
                    END IF
               END IF
!
               IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! ----------------- Timing printout
!
                    CALL TIM_GET ( 'PROC-02' )
                    CALL TIM_INIT()
               END IF
               GOTO 910 ! Well, go there again.
            ELSE IF ( SNGCHK_CMP .EQ. SNGCHK_CMP__STOP ) THEN
               STOP 'PROC(loop) Abnormal termination: singulairy check'
            ELSE IF ( SNGCHK_CMP .EQ. SNGCHK_CMP__SKIP ) THEN
               CONTINUE
            ELSE IF ( SNGCHK_CMP .EQ. SNGCHK_CMP__FAIL ) THEN
               CALL ERR_LOG ( 8386, IUER, 'LOOP', 'Trap of '// &
     &             'internal control: failure to determine completion '// &
     &             'status of SNGCHK while the database '//B3DOBJ%DBNAME_MES// &
     &             ' was processed. Please send '// &
     &             'verbose bug report to Leonid Petrov '// &
     &             '(Leonid.Petrov@nasa.gov)' )
               RETURN
          END IF
!
 1009     CONTINUE
          IDBGN = IDBEND(IDB) + 1
          IF ( SOLVE_DEBUG(1:1) == '7' .OR. &
     &         SOLVE_DEBUG(1:8) == 'GET_IONO' ) THEN
               CLOSE ( LUN_DEBUG )
               WRITE ( 6, '(A)' ) 'Wrote into debugging file '// &
     &                            FINAM_DEBUG(1:I_LEN(FINAM_DEBUG))// &
     &                            ' for session '//DBNAME_CH
          END IF
      END DO   ! end of master loop over data bases!
!@    write ( 6, * ) 'loop-3226 ' ; call flush ( 6 ) ! %%%%%%%%%%
!
      IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! -------- Timing printout
!
           CALL TIM_GET ( 'PROC-03' )
           CALL TIM_INIT()
      END IF
      IF ( FAST_DBG .EQ. F__PRI ) THEN
!
! -------- Debugging printout
!
           WRITE ( 6, 240 )  b3dobj%dbname_mes, nparam, b3dobj%nobs_t, &
     &     b3dobj%nobs_a
 240       format ( 1x,' Farwell LOOP:   ',A,' nparam=',i5,' l_obs=',i5, &
     &                 ' lr_obs=',i5 )
      END IF
!
! --- Closing observation file
!
!@   write ( 6, * ) 'loop-3240 ' ; call flush ( 6 ) ! %%%%%%%%%%
      CALL ACS_OBSFIL ( 'C' )
!
      IF ( FL_TPD_VERB ) THEN
           WRITE ( 6, * ) 'loop-2548 TPD_FLAG = ', TPD_FLAG, ' TPD_USE_FLAG= ', TPD_USE_FLAG, &
     &                    ' FL_TPD_READ= ', FL_TPD_READ, ' FL_TPD_WRITE= ', FL_TPD_WRITE
           WRITE ( 6, * ) 'loop-2549 TPD%HEADER%RATE_USE = ', TPD%HEADER%RATE_USE 
           WRITE ( 6, * ) 'loop-2550 allc= ', ASSOCIATED ( TPD%STA ), ASSOCIATED ( TPD%PARAM ), &
     &                                        ASSOCIATED ( TPD%DELAY ), ASSOCIATED ( TPD%RATE )
           WRITE ( 6, '(//A)' ) ' '
           CALL PAUSE ( 'PROC(loop)-2547' )
      END IF
      IF ( FL_TPD_WRITE ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL TPD_WRITE ( TPD, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8387, IUER, 'LOOP', 'Failure during '// &
     &              'attempt to write TPD data strucuture while '// &
     &              'experiment '//B3DOBJ%DBNAME_MES//' was beging '// &
     &              'processing' )
                RETURN
           END IF
         ELSE IF ( FL_TPD_READ ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL TPD_QUIT ( TPD, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8388, IUER, 'LOOP', 'Error in an '// &
     &              'attempt to release memory grabed by TPD while the '// &
     &              'experiment '//B3DOBJ%DBNAME_MES//' was beging processed' )
                RETURN
           END IF
      END IF
!
      IF ( ILEN(VTD_CONF_USE) > 0 .AND. .NOT. FL_TPD_READ ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_QUIT ( %VAL(VTD_ADR), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8389, IUER, 'LOOP', 'Error in VTD_QUIT' )
                RETURN
           END IF
           IF ( MEM_LEN .NE. 0 ) THEN
                CALL FREE ( MEM_ADR )
                MEM_LEN = 0
                MEM_ADR = 0
           END IF
      END IF
      IF ( IONOV_USE == IONOV__GEN .AND. .NOT. FILE_IONO_EX ) THEN
           CLOSE ( UNIT=LUN_IONO )
      END IF
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, * ) ' PROC/LOOP finished for '//B3DOBJ%DBNAME
      END IF
!!      write ( 6, * ) 'loop-3298 UTC_M_TAI = ', UTC_M_TAI ; call flush ( 6 ) ! %%%%%%%%%%
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  LOOP  !#!#
