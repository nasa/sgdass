!This is the start of file GLBC4
!
! Automatically generated on 2024.11.30_21:43:54 from glbc4_i.templ
!
! Last update:  2024.11.30_12:47:08
!
! modifications:
!
! 95/07/17 kdb Fix apparent error in size of free space buffer
!              (change from 185 to 177, to fix failure to decrease properly
!              when min_et was added.)
! 95/07/27 kdb  Increase the size of MAX_SDC from 50 to 100.
! 96/11/12 kdb  Increase part_array dimension from 7 to 112.
! 97/01/27 pet  Variables FAST_MODE, FAST_DBG added
! 97/01/28 pet  Parameters FAST_MODE__DEF, FAST_DBG__DEF added
! 97/03/03 pet  Variable FAST_COV and parameter FAST_COV__DEF added
! 97/03/17 pet  Added beginning and ending marks
! 97.05.13 pet  Variable FAST_MODE_GLO, FAST_DBG_GLO, FAST_COV_GLO added
! 97.07.08 pet  Split parameter FAST_COV__DEF on FAST_COV__DEF_B  for BATCH
!               mode and FAST_COV__DEF_I for interactive mode
! 97.07.12 pet  Added variables  CHI_TOL, TIGHTNESS, REWAY_VERBOSE
!               Added parameters CHI_TOL__DEF, TIGHTNESS__DEF,
!               REWAY_VERBOSE__DEF, REWAY_TYPE__DEF
! 97.08.08 pet  Added GAMB variables and constants:
!               GAMB_F_ION,  GAMB_F_PREUSE,      GAMB_F_SAVE,
!               GAMB_CUTOFF, GAMB_SPACING_CONST, GAMB_MINOBS, GAMB_IT,
!               GAMB_BATCH
! 97.08.10 pet  Added IONO variables and constants: IONO_VERBOSE, IONO_SEEK,
!               IONO_VERBOSE__DEF, IONO_SEEK_DEF. Increased the size at 256
!               bytes.
! 97.08.12 pet  Added  QUALCODE_GOOD_LIM  and  QUALCODE_GOOD_LIM__DEF  global
!               variables.
! 97.08.17 pet  Added  SOLVE_EDITOR and SOLVE_PS_VIEWER variables.
! 97.09.17 pet  Added  variables: ELIM_MOD, ELIM_CNF, ELIM_THR, ELIM_CUT,
!                                 ELIM_VRB, ELIM_TYP and
!               the constants which corresponds them
! 97.09.30 pet  Added variables: SOLVE_EMULATION, constant SOLVE_EMULATION__DEF,
!                                COND_WARNING,    constant COND_WARNING__DEF
! 97.10.06 pet  Added variables: SEG_OUTPUT    and constant SEG_OUTPUT__DEF
! 97.10.30 pet  Added variables: ELIM_UPD      and constant ELIM_UPD__DEF
! 97.11.05 pet  Added ATM_CNST__DEF, CLO_CNST__DEF, POL_CNST__DEF,
!                     UT1_CNST__DEF, GRR_CNST__DEF, GRO_CNST__DEF  constants
! 97.11.17 jwr  Added MAPPED_EOP_OUTPUT.
! 97.12.12 pet  Added variables ESTIMATE_NUTATION_FIRST,
!               ESTIMATE_UT1_RATE_FIRST, ESTIMATE_STATION_FIRST,
!               ESTIMATE_EOP_FIRST  and corresponding them constants
!               ESTIMATE_NUTATION_FIRST__DEF, ESTIMATE_UT1_RATE_FIRST__DEF,
!               ESTIMATE_STATION_FIRST__DEF, ESTIMATE_EOP_FIRST__DEF
! 98.01.21 pet  Removed variables CHI_TOL, TIGHTNESS, REWAY_VERBOSE and
!               parameters CHI_TOL__DEF, TIGHTNESS__DEF.
!               New variables REWAY_FLODEL, REWAY_FLORATE, REWAY_CEIDEL,
!               REWAY_CEIRATE, REWAY_CHITOL, REWAY_MAXIT,
!               REWAY_NEWUPD,  REWAY_FALLBACK and corresponding them parameters
!               are added
! 98.02.24 pet  Added variables MAPPED_EOP_TIME, SAVING_RATE,
!               RCOND_CGM   and constant SAVING_RATE__DEF
! 98.03.20 pet  Added variables ELIM_AMB, ELIM_ION, PAMB_VRB, PAMB_PARU_FILE,
!               PAMB_PLOT_TYPE, PAMB_PLOT_BAND  and corresponding constants
! 98.05.01 pet  Added logical variables CRES_PRE98, CNPLT_SUPR_PRE98,
!                                       TITLE_ANON, TITLE_PRE98
! 98.05.02 pet  Added logical variables CNPLT_SUPR_SIMPLE,
!                                       CNPLT_SHOW_CBAD, CNPLT_SHOW_UNRC
! 98.05.05 pet  Added logical constants TITLE_ANON__DEF,
!               CNPLT_SHOW_CBAD__DEF, CNPLT_SHOW_UNRC__DEF
! 98.05.08 pet  Added variable SUPMET_BAT
! 98.05.10 pet  Increased size from 98 to 99 256-bytes blocks
! 98.07.22 pet  Added 26 sigmas of constraints
!               Increased size from 99 to 100 256-bytes blocks
! 98.08.07 pet  Added logical variable CNPLT_PHASE_S and constant
!               CNPLT_PHASE_S__DEF
! 98.11.02 pet  Added variable PAMB_PSL_TYPE
! 98.11.21 pet  Added variables: ELIM_MSR      and constant ELIM_MSR__DEF
!               Added PAMB_XGRLIM, PAMB_SGRLIM, PAMB_XPHLIM, PAMB_SPHLIM,
!                     PAMB_DEFRG,  PAMB_ARFMS,  PAMB_RLIM3,  PAMB_FRZTR,
!                     PAMB_SPLSPAN, PAMB_SPL_CNST, PAMB_MSC, PAMB_ARFTYPE,
!                     PAMB_PSTA, PAMB_FSTA, PAMB_PLOTINI, PAMB_PLOTINI,
!                     PAMB_INIWEI, PAMB_ARFFLO  and related constant
! 98.12.22 pet  Added variables: DEF_CLOSPAN, DEF_ATMSPANB and related constants
! 99.01.01 pet  Added variables: TRAIN, GLO_PARLIM, INC_PARLIM, SORT_SOU,
!                                SORT_STA and related constants
! 99.04.06 pet  Added logical variable G_WARNING and related constant
! 1999.05.13  pet  Added variable: MEMFAULT_REP
! 1999.07.23  pet  Added variable: WEB_DIR, WEB_DIR__DEF
! 1999.07.27  pet  Added variable: MDLPL_IPS_PAG, MDLPL_IPC_PAG,
!                                  MLDLP_FL_EOPMOD, MDLPL_FL_CLF
!                                  and corresponding constants
! 1999.08.17  pet  Added variable: MDLPL_FL_FRAME and a corresponding constant
! 1999.09.29  pet  Added variable: COR_FLAG, COR_GG_FLAG, COR_GL_FLAG,
!                             COR_LL_FLAG, COR_CL_FLAG, COROUT_FORM,
!                             COR_GG_INCFIL, COR_GG_EXCFIL,
!                             COR_GL_INCFIL, COR_GL_EXCFIL,
!                             COR_LL_INCFIL, COR_LL_EXCFIL,
!                             COR_CL_INCFIL, COR_CL_EXCFIL,
!                             COR_CL_INCSES, COR_CL_EXCSES
!                  for support of a new subroutine CORRELATION
! 1999.10.07  pet  Added variable: CONTROL_FILE
! 1999.10.15  pet  Added variable: KECC
! 2000.01.24  pet  Added variable: NORATE_FLAG
! 2000.03.27  pet  Added variable: EQUMEM_FLAG
! 2000.03.29  pet  Added constants WEIGHT__NONE, WEIGHT__MYWAY, WEIGHT__UPWEI,
!                                  WEIGHT__UPWEI_OPT
! 2000.04.04  pet   Added constants UPWEI_FLOOR_GROUP__BATCH,
!                                   UPWEI_FLOOR_PHASE__BATCH,
!                                   UPWEI_CHITOL__BATCH
! 2000.05.01  pet   Added variable  APRIORI_ZENDEL and constant
!                   APRIORI_ZENDEL__DEF
! 2000.09.22  pet   Added variable: KMGR
! 2000.09.25  pet   Added variable: SUP_CONFIG
! 2000.11.22  pet   Removed variables VELCON, DEFVELCNST, ALTVELCNST
!             pet   Added variables STAXYZ_CNST, STAXYZ_CNSB, STAXYZ_CNFL,
!                                   STAUEN_CNST, STAUEN_CNSB, STAUEN_CNFL,
!                                   VELXYZ_CNST, VELXYZ_CNSB, VELXYZ_CNFL,
!                                   VELUEN_CNST, VELUEN_CNSB, VELUEN_CNFL
! 2001.01.12  pet   Added variable: KMET
! 2001.05.29  pet   Added equivalence to character form of variable EOPDLY
! 2002.03.27  pet   Added varaibles FL_SINEX_MAKE, FL_SINEX_GLO,  FL_SINEX_LOC,
!                                   FL_SINEX_SEG,  SINEX_OUTFILE, SINEX_INCLUDE,
!                                   SINEX_EXCLUDE, FL_SINEX_EST,  FL_SINEX_COV,
!                                   FL_SINEX_CNS,  FL_SINEX_DCM,
!                                   SINEX_ACKFIL,  SINEX_COMFIL, SINEX_VERS
! 2002.03.28  pet   Added varaibles FL_NRD_TABLE, FL_CHI_TABLE
! 2002.03.28  pet   Added constants FL_NRD_TABLE__DEF, FL_CHI_TABLE__DEF
! 2002.04.08  pet   Added constants related to formatting listings
!                   in SINEX format.
! 2002.05.06  pet   Added variables SINEX_REALNAME, STAT_NUMOBS, STAT_NUMUNK,
!                                   STAT_SQUOC,  STAT_SQURES, STAT_VARFAC,
!                                   STAT_WRMS
! 2002.05.08  pet   Added constatins related for definition of constraints to
!                   be out in Sinex listing output
! 2002.05.10  pet   Added variables NNT_POS_RTP, NNR_POS_RTP,
!                                   NNT_VEL_RTP, NNR_VEL_RTP  -- vectors of
!                   right hand part of equations of constraints
! 2002.05.17  pet   Added constant PRM_REF_YR -- default reference date for
!                   source propser motion
! 2002.05.30  pet   Removed FL_SINEX_NRM variable and added SINEX_VERS varaible
!                   and paramters N__SNX__VERS, SNX__VERS
! 2002.09.27  pet   Added arrays POSVAR_FIL, POSVAR_MOD, POSVAR_INT, POSVAR_USE
! 2002.10.03  pet   Added variable N_POSVAR
! 2002.12.13  pet   Added variables for position variations:
!                   N_PSVHAR  , N_PSVSTA  , STS_NAMSIT, ADR_NAMSIT,
!                   LEN_NAMSIT, STS_HARVAL, ADR_HARVAL, LEN_HARVAL,
!                   STS_HARDSP, ADR_HARDSP, LEN_HARDSP, STS_BDSSIT,
!                   STS_STACOO, ADR_STACOO, LEN_STACOO, STS_BDSFIL,
!                   ADR_BDSFIL, LEN_BDSFIL, STS_BDSSAM, ADR_BDSSAM,
!                   LEN_BDSSAM, STS_BDSFMJ, ADR_BDSFMJ, LEN_BDSFMJ,
!                   STS_BDSFSC, ADR_BDSFSC, LEN_BDSFSC, STS_BDSLMJ,
!                   ADR_BDSLMJ, LEN_BDSLMJ, STS_BDSLSC, ADR_BDSLSC,
!                   LEN_BDSLSC, STS_BDSNSA, ADR_BDSNSA, LEN_BDSNSA
! 2002.12.16  pet   Added variable TIM_PSVFIL
! 2002.12.23  pet   Added array BDS_ENDIAN
! 2002.12.24  pet   Removed constant PRM_REF_YR
! 2002.12.23  pet   Added array BDS_FLOAT
! 2003.08.12  pet   Removed CRES_PRE98, TITLE_ANON, TITLE_PRE98
!                   Added CRES_STYLE
! 2003.08.15  pet   Moved MAX_FLYBY_EOP_VALUES  to solve.i
! 2003.09.02  pet   Added NNR_PRP related variables
! 2003.09.02  pet   Added L_HEO, ADR_HEO, STAT_HEO, FINAM_HEO, NAME_HEO,
!                         NUT_EXPANSION variables
! 2004.03.09  pet   Added VTD_CONF_FILE, VTD_ADR, VTD_STATUS
! 2004.03.11  pet   Added variable HEO_EPOCH_SEC, removed NUT_EXPANSION
! 2004.10.20  pet   Defined character equivalents for 10 integer*2 arrays
! 2004.11.18  pet   Moved PSV-constants to bindisp.i and harpos.i
!                   Renamed M__POSVAR to MAX__POSVAR
! 2004.11.24  pet   Added variables STASUB2_CHR, VELSUB2_CHR, PLTSUB2_CHR,
!                   ECCSUB_CHR,  MGRSUB_CHR,  METSUB_CHR,  PLATE_SCL,
! 2005.01.27  pet   Added variables FRINGE_ROOT_DIR
! 2005.02.21  pet   Added variables L_HPE, L_SPE, ADR_HPE, ADR_SPE
! 2005.03.15  pet   Added variables L_BSP, ADR_BSP
! 2005.03.18  pet   Added variable SRC_LISTING_STYLE, STA_LISTING_STYLE,
!                                  BAS_LISTING_STYLE
! 2005.03.18  pet   Added constant SRC_LISTING_STYLE__DEF
! 2005.03.25  pet   Added array FL_WARN_POSVAR
! 2005.03.28  pet   POSVAR_RD_AREA
! 2005.06.22  pet   Added constants related to HPNT, HPNR, SPPS and SPVL
!                   constraints
! 2005.07.21  pet   Added variable SEG_LISTING_STYLE
! 2005.07.21  pet   Added constant SEG_LISTING_STYLE__DEF
! 2006.01.24  pet   Added variables,
!                   L_EERM, ADR_EERM, L_MERM, ADR_MERM, FINAM_MERM
! 2006.02.08  pet   Added variable FL_NOFLYBY, FL_VTD_GLB, FL_VTD_SES
! 2006.02.09  pet   Added variable VTD_CONF_SES
! 2006.03.24  pet   Added variable BATCH_CNF_FINAM
! 2006.05.04  pet   Added constants DEF__WOBBLE_SIGMA_MAS and 
!                   DEF__UT1_SIGMA_MSEC   -- default uncertainteis for
!                   external EOP time series
! 2006.05.25  pet   Added variables L_EHEO, ADR_EHEO, MJD_EHEO_REF, 
!                   TAI_EHEO_REF
! 2006.06.25  pet   Added variables L_EHEC, ADR_EHEC
! 2006.11.06  pet   Added variables L_AEM,  ADR_AEM
! 2007.03.15  pet   Added variable  ADR_EHES
! 2007.06.01  pet   Added constant  WEIGHT__ELIM
! 2007.07.30  pet   Added definition of SRC_COO_SIGMA__DEF 
! 2007.08.09  pet   Added definition of SOU_ADM_FLAG and SOU_ADM_CNS
! 2007.10.01  pet   Added variable  SUPRESS_FILE
! 2007.10.25  pet   Added variables EDC_USE, EDC_PAR, EDC_DIR
! 2007.11.06  pet   Added variables TPD_FLAG, TPD_DIR
! 2007.11.09  pet   Added variables RWT_EL_USE, RWT_SRC_USE, RWT_EL_GLB, RWT_STA_EL_FIL
! 2007.12.03  pet   Added variables RWT_SRC_FIL
! 2008.02.04  pet   Added variables TRP_DIR, TRP_USE,
!                                   N_FIL_TRP, ADR_TRP_FIL_BUF, STS_TRP_FIL,
!                                   ADR_TRP_SES_BUF, STS_TRP_SES, 
!                                   ADR_TRP, STS_TRP
! 2008.03.22  pet   Added variables NUT_USE_CODE
! 2008.04.21  pet   Added variables ATD_USE, ATD_FIL, ATD_MEA_USE, ATD_MEA_FIL,
!                                   ATD_ADR, ATD_STS
! 2008.04.21  pet   Added variables AOC_USE, AOC_DIR
! 2008.04.21  pet   Added constants NTX_SNX, NTY_SNX
! 2008.08.06  pet   Added constants SUPMET_DEF
! 2009.04.30  pet   Added constant  BRK_CNST__DEF
! 2010.02.07  pet   Replaced SOLVE_PS_VIEWER variable with OLD_SOLVE_PS_VIEWER 
! 2010.06.10  pet   Added variables SNR_MIN_X, SNR_MIN_S
! 2010.10.21  pet   Added variable  TEC_NOISE_FILE
! 2010.10.22  pet   Added variable  SEED_INIT_I4
! 2010.11.04  pet   Added variable  FL_NO_IONO_DEL
! 2013.06.11  pet   Added variables GLO_SNR_MIN_X, GLO_SNR_MIN_S
! 2018.01.08  pet   Changed CNPLT_SHOW_UNRC__DEF to .FALSE.
! 2018.09.07  pet   Added variable IONO_ERR_FCT
! 2018.11.11  pet   Added variable FL_EQUAL_EFF_FREQ
! 2018.11.13  pet   Added variable ERR_FUDGE_FACTOR
! 2018.11.13  pet   Added variable FRTYPFIT
! 2019.09.13  pet   Added variable FL_EQUAL_EFF_FREQ 
! 2020.04.27  pet   Added variable EDIT_FIL
! 2020.07.14  pet   Added variable ADDW_FIL
! 2021.05.17  pet   Added variable ASM_NAM and ASM_STA_FIL
! 2021.12.30  pet   Added variables SESS_REWEI_SCALE and SESS_REWEI_QUADR, AOC_FIL
! 2021.12.30  pet   removed ERR_FUDGE_FACTOR, AOC_USE, AOC_DIR
! 2021.12.31  pet   Added DTEC_FIL and DTEC_USE
! 2022.06.26  pet   Added TEC_BIAS, TEC_SCAL
! 2022.09.02  pet   Added ADDW_SCL
! 2022.12.06  pet   Added variables GIM_INFO_DIR, GIM_DTEC_DIR, BCL_FIL, BRK_FIL, 
!                                   GIM_MODE, GIM_DEG, GIM_TIM_STEP, GIM_SCALE, 
!                                   GIM_COLLECT_INFO, GIM_EST, GIM_WRI, GIM_VERB
! 2022.12.09  pet   Added variables GIM_ADDW_DIR, GIM_DEL_DIR, GIM_NOI_DIR, GIM_RGR, GIM_SEED
! 2023.01.06  pet   Added variables DTEC_SBA_USE, DTEC_ERR_SCL
! 2023.02.10  pet   Added variables EXT_ERR_FIL
! 2023.03.12  pet   Added variables SRC_PRP_SIGMA
! 2023.12.22  pet   Added variables VCAT_CONF_FILE, VCAT_REPO
! 2024.11.30  pet   Added varaible  ADDW_USE
!
!-----
!
      INTEGER*4  BEGMARK_GLBC4_I4, ENDMARK_GLBC4_I4, LEN_GLBC4_FILLER
      PARAMETER  ( LEN_GLBC4_FILLER = 210 ) ! unused free space in BYTES !! NB!
      INTEGER*1    GLBC4_FILLER
!
! --- Common SOLVE constants
!
      INTEGER*4   UNDEF__SRT, NO__SRT, ALPHB__SRT, ASCEN__SRT, LONG__SRT
      PARAMETER ( UNDEF__SRT =  -1 )
      PARAMETER ( NO__SRT    = 801 )
      PARAMETER ( ALPHB__SRT = 802 )
      PARAMETER ( LONG__SRT  = 803 )
      PARAMETER ( ASCEN__SRT = 804 )
!
      INTEGER*4   QUALCODE_GOOD_LIM__DEF, SOLVE_EMULATION__DEF, &
     &            SAVING_RATE__DEF, &
     &            GLO_PARLIM__DEF, GLO_PARLIM__LIM, INC_PARLIM__DEF, &
     &            SORT_STA__DEF, SORT_SOU__DEF
      REAL*8      COND_WARNING__DEF
      LOGICAL*4   SEG_OUTPUT__DEF, TRAIN__DEF
      CHARACTER   SOLVE_EDITOR__DEF*16, SOLVE_PS_VIEWER__DEF*16
      PARAMETER ( QUALCODE_GOOD_LIM__DEF =  8 ) ! Good qualcode
      PARAMETER ( SOLVE_EMULATION__DEF   =  0 ) ! Proper mode
      PARAMETER ( SOLVE_EDITOR__DEF      =  'vi              ' )
      PARAMETER ( SOLVE_PS_VIEWER__DEF   =  'GhosT           ' )
      PARAMETER ( COND_WARNING__DEF      =  1.D11              )
      PARAMETER ( SEG_OUTPUT__DEF        =  .FALSE.            )
      PARAMETER ( SAVING_RATE__DEF       =  1 ) ! Solution saving frequency
      PARAMETER ( TRAIN__DEF             = .TRUE. ) ! Use train SOLVE
      PARAMETER ( GLO_PARLIM__DEF        = 2048   ) ! number of global param
      PARAMETER ( GLO_PARLIM__LIM        = 256    ) ! min acceptable glo_parlim
      PARAMETER ( INC_PARLIM__DEF        = 128    ) ! increment of num of param
      PARAMETER ( SORT_STA__DEF = NO__SRT )
      PARAMETER ( SORT_SOU__DEF = NO__SRT )
      LOGICAL*4   ESTIMATE_NUTATION_FIRST__DEF, &
     &            ESTIMATE_UT1_RATE_FIRST__DEF, &
     &            ESTIMATE_STATION_FIRST__DEF, &
     &            ESTIMATE_EOP_FIRST__DEF, NORATE__DEF, EQUMEM__DEF, &
     &            FL_NRD_TABLE__DEF, FL_CHI_TABLE__DEF
      LOGICAL*4   APRIORI_ZENDEL__DEF
      PARAMETER ( APRIORI_ZENDEL__DEF = .FALSE. ) ! not to compute full
!                                      ! contribution of atmosphere path delay
      PARAMETER ( FL_NRD_TABLE__DEF = .TRUE.  ) ! NRD statistic table -> listing
      PARAMETER ( FL_CHI_TABLE__DEF = .FALSE. ) ! Chi-square table -> listing
      INTEGER*4    SUPMET_DEF
      PARAMETER  ( SUPMET_DEF = SUPMET__PRE98 ) ! Default suppression method
!
! --- Default constants for setting estimation model in interactive SOLVE
! --- when database or superfile is read
!
      PARAMETER ( ESTIMATE_NUTATION_FIRST__DEF = .FALSE.  )
      PARAMETER ( ESTIMATE_STATION_FIRST__DEF  = .FALSE.  )
      PARAMETER ( ESTIMATE_EOP_FIRST__DEF      = .FALSE.  )
      PARAMETER ( ESTIMATE_UT1_RATE_FIRST__DEF = .FALSE.  )
      PARAMETER ( NORATE__DEF                  = .FALSE.  )
      PARAMETER ( EQUMEM__DEF                  = .TRUE.   )
!
      LOGICAL*2   G_WARNING__DEF
      PARAMETER ( G_WARNING__DEF = .FALSE. ) ! Deafault warning
!
! --- FAST mode constants
!
      INTEGER*4   FAST_MODE__DEF,  FAST_DBG__DEF, &
     &            FAST_COV__DEF_B, FAST_COV__DEF_I
      PARAMETER ( FAST_MODE__DEF  = 2 ) ! B3D mode is default
      PARAMETER ( FAST_DBG__DEF   = 0 ) ! debug printout disabled
      PARAMETER ( FAST_COV__DEF_B = 2 ) ! calculate local and global cov.
!                                       ! matrices in fast mode for BATCH runs
      PARAMETER ( FAST_COV__DEF_I = 3 ) ! calculate all elementts of cov.
!                                       ! matrix in interctive mode
! --- SDBH constants
!
      REAL*8     ATM_CNST__DEF, &   !  Default atmosphere rate constraint sigma
     &           CLO_CNST__DEF, &   !  Default rate constraint sigma
     &           POL_CNST__DEF, &   !  Default pole coordinate rate constraint sigma
     &           UT1_CNST__DEF, &   !  Default UT1 argument sigma
     &           GRR_CNST__DEF, &   !  Default gradient rate constraint sigma
     &           GRO_CNST__DEF, &   !  Default gradient offset constraint sigma
     &           SRC_COO_SIGMA__DEF, & ! Default source coordinate signa
     &           SRC_PRP_SIGMA__DEF, & ! Default source proper motion signa
     &           BRK_CNST__DEF         ! Default clock breaks/clock function constraint sigma
      PARAMETER  ( ATM_CNST__DEF = 50.0  ) ! psec/hour
      PARAMETER  ( CLO_CNST__DEF =  5.0  ) ! 1.D-14 sec/sec
      PARAMETER  ( POL_CNST__DEF = 10.0  ) ! mas/day
      PARAMETER  ( UT1_CNST__DEF =  0.67 ) ! msec/day
      PARAMETER  ( GRR_CNST__DEF =  0.0  ) ! mm/day
      PARAMETER  ( GRO_CNST__DEF =  0.0  ) ! mm
      PARAMETER  ( SRC_COO_SIGMA__DEF = 0.0   ) ! rad
      PARAMETER  ( SRC_PRP_SIGMA__DEF = 1.D-8 ) ! rad/year
      PARAMETER  ( BRK_CNST__DEF = 1.D-9 ) ! dimensionless
!
! --- REWAY constants
!
      REAL*8      REWAY_FLODEL__DEF,   REWAY_FLORATE__DEF, &
     &            REWAY_CEIDEL__DEF,   REWAY_CEIRATE__DEF, &
     &            REWAY_CHITOL__DEF
      REAL*8      UPWEI_FLOOR_GROUP__BATCH, UPWEI_FLOOR_PHASE__BATCH, &
     &            UPWEI_CHITOL__BATCH
      INTEGER*2   REWAY_MAXIT__DEF
      LOGICAL*2   REWAY_VERBOSE__DEF, REWAY_FALLBACK__DEF, &
     &            REWAY_NEWUPD__DEF
      CHARACTER   REWAY_TYPE__DEF*2
!
      PARAMETER ( REWAY_FLODEL__DEF  = 0.0    ) ! (psec) Floor for delay
      PARAMETER ( REWAY_FLORATE__DEF = 0.0    ) ! (fs/s) Floor for rate
      PARAMETER ( REWAY_CEIDEL__DEF  = 300.0  ) ! (psec) Ceiling for delay
      PARAMETER ( REWAY_CEIRATE__DEF = 1000.0 ) ! (fs/s) Ceiling for rate
      PARAMETER ( REWAY_CHITOL__DEF  = 0.005  ) ! Tolerance factor for Chisq/ndf
      PARAMETER ( REWAY_MAXIT__DEF   = 10     ) ! Maximum number of iterations
      PARAMETER ( REWAY_VERBOSE__DEF = .TRUE. ) ! Verbosity mode for
!                                               ! interactive reway
      PARAMETER ( REWAY_FALLBACK__DEF= .TRUE. ) ! Fall back mode:
!                                               ! baseline-->station-->global
      PARAMETER ( REWAY_NEWUPD__DEF  = .FALSE.) ! New (for 21-JAN-98) weights
!                                               ! update mode
      PARAMETER ( REWAY_TYPE__DEF    = 'NO'   ) ! Default type of reweighting
!
! --- This parameters for batfch ELIM(UPWEI). They are not used when ELIM(UPWEI)
! --- is called in interactive mode
!
      PARAMETER ( UPWEI_FLOOR_GROUP__BATCH = 8.0 )  ! (psec) Floor batch UPWEI
      PARAMETER ( UPWEI_FLOOR_PHASE__BATCH = 3.0 )  ! (psec) Floor batch UPWEI
      PARAMETER ( UPWEI_CHITOL__BATCH =  0.02 ) ! Tolerance factor batch UPWEI
!
      INTEGER*4  WEIGHT__NONE, WEIGHT__MYWAY, WEIGHT__UPWEI, &
     &           WEIGHT__UPWEI_OPT, WEIGHT__ELIM
      PARAMETER  ( WEIGHT__NONE      = 25891 )
      PARAMETER  ( WEIGHT__MYWAY     = 25892 )
      PARAMETER  ( WEIGHT__UPWEI     = 25893 )
      PARAMETER  ( WEIGHT__UPWEI_OPT = 25894 )
      PARAMETER  ( WEIGHT__ELIM      = 25895 )
!
! --- GAMB constants
!
      LOGICAL*4  GAMB_F_X_BAND__DEF, GAMB_F_S_BAND__DEF, &
     &           GAMB_F_ION__DEF,    GAMB_F_PREUSE__DEF, &
     &           GAMB_F_SAVE__DEF,   GAMB_BATCH__DEF
      REAL*8     GAMB_CUTOFF__DEF,   GAMB_SPACING_CONST__DEF
      INTEGER*4  GAMB_MINOBS__DEF,   GAMB_IT__DEF
      PARAMETER ( GAMB_F_X_BAND__DEF      = .TRUE.  )
      PARAMETER ( GAMB_F_S_BAND__DEF      = .TRUE.  )
      PARAMETER ( GAMB_F_ION__DEF         = .TRUE.  )
      PARAMETER ( GAMB_F_PREUSE__DEF      = .FALSE. )
      PARAMETER ( GAMB_F_SAVE__DEF        = .TRUE.  )
      PARAMETER ( GAMB_CUTOFF__DEF        = 8.0D-9  )
      PARAMETER ( GAMB_MINOBS__DEF        = 8       )
      PARAMETER ( GAMB_SPACING_CONST__DEF = 0.0D0   )
      PARAMETER ( GAMB_IT__DEF            = 2       )
      PARAMETER ( GAMB_BATCH__DEF         = .FALSE. )
!
! --- IONO constants
!
      LOGICAL*4    IONO_VERBOSE__DEF             ! verbosity mode for IONO
      INTEGER*4    IONO_SEEK__DEF                ! S-band search window
      PARAMETER  ( IONO_VERBOSE__DEF = .FALSE. )
      PARAMETER  ( IONO_SEEK__DEF    = 25      )
!
! --- SETFL constants
!
      INTEGER*4    SETFL_MDEG__DEF
      REAL*8       DEF_CLOSPAN__DEF, DEF_ATMSPAN__DEF
      PARAMETER  ( SETFL_MDEG__DEF   = 2 ) ! Max degree of global clock polynom
      PARAMETER  ( DEF_CLOSPAN__DEF = 3600.D0 ) ! Default duration of clock
!                                               ! span for one segment (in sec)
      PARAMETER  ( DEF_ATMSPAN__DEF = 3600.D0 ) ! Default duration of atm. span
!
! --- ELIM constants
!
      LOGICAL*4  ELIM_MOD__DEF, ELIM_CNF__DEF, ELIM_AMB__DEF, &
     &           ELIM_ION__DEF
      REAL*8     ELIM_THR__DEF, ELIM_CUT__DEF, ELIM_MSR__DEF
      INTEGER*4  ELIM_VRB__DEF, ELIM_UPD__DEF
      CHARACTER  ELIM_TYP__DEF*2
      PARAMETER  ( ELIM_MOD__DEF = .TRUE.  )
      PARAMETER  ( ELIM_CNF__DEF = .FALSE. )
      PARAMETER  ( ELIM_THR__DEF = 0.0D0   )
      PARAMETER  ( ELIM_CUT__DEF = 3.0D0   )
      PARAMETER  ( ELIM_VRB__DEF = 2       )
      PARAMETER  ( ELIM_TYP__DEF = 'BA'    )
      PARAMETER  ( ELIM_UPD__DEF = 1       )
      PARAMETER  ( ELIM_AMB__DEF = .FALSE. )
      PARAMETER  ( ELIM_ION__DEF = .FALSE. )
      PARAMETER  ( ELIM_MSR__DEF = 0.0D0   )
!
! --- CRES constants
!
      LOGICAL*1    TITLE_ANON__DEF
      PARAMETER  ( TITLE_ANON__DEF = .FALSE. )
!
! --- PROC constants
!
      INTEGER*4    SNGCHK_ACTION__DEF, SNGCHK_SOUMIN__DEF, &
     &             SNGCHK_STAMIN__DEF, SNGCHK_BASMIN__DEF
      PARAMETER  ( SNGCHK_ACTION__DEF = SNGCHK_ACT__WARN ) ! Default: warning
      PARAMETER  ( SNGCHK_SOUMIN__DEF = 0                ) ! Default:
      PARAMETER  ( SNGCHK_STAMIN__DEF = 0                ) !   switch off the
      PARAMETER  ( SNGCHK_BASMIN__DEF = 0                ) !   control
!
! --- CNPLT constants
!
      LOGICAL*1    CNPLT_SHOW_UNRC__DEF, &
     &             CNPLT_SHOW_CBAD__DEF, &
     &             CNPLT_PHASE_S__DEF
      PARAMETER  ( CNPLT_SHOW_UNRC__DEF = .FALSE.  )
      PARAMETER  ( CNPLT_SHOW_CBAD__DEF = .TRUE.  )
      PARAMETER  ( CNPLT_PHASE_S__DEF   = .FALSE. )
!
! --- PAMB constants
!
      INTEGER*4  PAMB_VRB__DEF,      PAMB_PLOT_TYPE__DEF, &
     &           PAMB_PSL_TYPE__DEF, PAMB_PLOT_BAND__DEF, &
     &           PAMB_ARFTYPE__DEF
      PARAMETER  ( PAMB_VRB__DEF       = 1 )
      PARAMETER  ( PAMB_PLOT_TYPE__DEF = 1 )
      PARAMETER  ( PAMB_PSL_TYPE__DEF  = 1 )
      PARAMETER  ( PAMB_PLOT_BAND__DEF = 1 )
      PARAMETER  ( PAMB_ARFTYPE__DEF   = 3 )
!
      CHARACTER   PAMB_PARU_FILE__DEF*32
      PARAMETER ( PAMB_PARU_FILE__DEF = &
     &           '                                ' )
      REAL*8     PAMB_XGRLIM__DEF,  PAMB_SGRLIM__DEF, &
     &           PAMB_XPHLIM__DEF,  PAMB_SPHLIM__DEF, &
     &           PAMB_DEFRG__DEF,   PAMB_ARFMS__DEF, &
     &           PAMB_RLIM3__DEF,   PAMB_FRZTR__DEF, &
     &           PAMB_SPLSPAN__DEF, PAMB_SPL_CNST__DEF, &
     &           PAMB_INIWEI__DEF,  PAMB_ARFFLO__DEF
      PARAMETER  ( PAMB_XGRLIM__DEF   = 150.0D-12 )
      PARAMETER  ( PAMB_SGRLIM__DEF   = 600.0D-12 )
      PARAMETER  ( PAMB_XPHLIM__DEF   =  25.0D-12 )
      PARAMETER  ( PAMB_SPHLIM__DEF   = 100.0D-12 )
      PARAMETER  ( PAMB_DEFRG__DEF    =   0.3     )
      PARAMETER  ( PAMB_ARFMS__DEF    =   0.3     )
      PARAMETER  ( PAMB_RLIM3__DEF    =   0.0     )
      PARAMETER  ( PAMB_FRZTR__DEF    = 10800.D0  )
      PARAMETER  ( PAMB_SPLSPAN__DEF  =   2.0     )
      PARAMETER  ( PAMB_SPL_CNST__DEF =   0.05    )
      PARAMETER  ( PAMB_INIWEI__DEF   =   8.D-12  )
      PARAMETER  ( PAMB_ARFFLO__DEF   =   0.05D0  )
!
      LOGICAL*4  PAMB_MSC__DEF, PAMB_PLOTINI__DEF, PAMB_PLOTFIN__DEF
!
      PARAMETER  ( PAMB_MSC__DEF      =   .TRUE.  )
      PARAMETER  ( PAMB_PLOTINI__DEF  =   .FALSE. )
      PARAMETER  ( PAMB_PLOTFIN__DEF  =   .TRUE.  )
!
! --- MDLPL constatns
!
      CHARACTER  WEB_DIR__DEF*5
      INTEGER*4  MDLPL_IPS_PAG__DEF,   MDLPL_IPC_PAG__DEF
      LOGICAL*4  MDLPL_FL_EOPMOD__DEF, MDLPL_FL_CLF__DEF
      LOGICAL*4  MDLPL_FL_FRAME__DEF
!
      PARAMETER  ( WEB_DIR__DEF = '/tmp/' ) ! Web_dir is in /tmp as default
      PARAMETER  ( MDLPL_IPS_PAG__DEF   = 1      )
      PARAMETER  ( MDLPL_IPC_PAG__DEF   = 1      )
      PARAMETER  ( MDLPL_FL_EOPMOD__DEF = .TRUE. )
      PARAMETER  ( MDLPL_FL_CLF__DEF    = .TRUE. )
      PARAMETER  ( MDLPL_FL_FRAME__DEF  = .TRUE. )
!
! --- Default listing style
!
      INTEGER*4  SRC_LISTING_STYLE__DEF
      PARAMETER  ( SRC_LISTING_STYLE__DEF = SRC_PRE2004_SPOOL__FMT )
      INTEGER*4    SEG_PRE2005_SPOOL__FMT, SEG_POST2005_SPOOL__FMT, &
     &             SEG_LISTING_STYLE__DEF
      PARAMETER   ( SEG_PRE2005_SPOOL__FMT  = 12001 )
      PARAMETER   ( SEG_POST2005_SPOOL__FMT = 12002 )

      PARAMETER  ( SEG_LISTING_STYLE__DEF = SEG_PRE2005_SPOOL__FMT )
!
      REAL*8     PI__NUM_GLBC4, OM__EAR_GLBC4, RAD__TO__MAS_GLBC4
      PARAMETER  ( PI__NUM_GLBC4 = 3.141592653589793D0 )
      PARAMETER  ( OM__EAR_GLBC4 = 7.292115146706387D-05 ) ! rad/sec
      PARAMETER  ( RAD__TO__MAS_GLBC4 = 3600.D0*1000.D0*180.D0/PI__NUM_GLBC4 )
!
! --- Constants related to formatting listings in SINEX format
!
      INTEGER*4    N__SNX__VERS
      PARAMETER  ( N__SNX__VERS = 2 )
      CHARACTER  SNX__VERS(N__SNX__VERS)*4
      DATA       SNX__VERS &
     &           / &
     &             '2.10', &
     &             '2.20'  &
     &           /
!
      INTEGER*4    N__SNX
      PARAMETER  ( N__SNX = 21 )
      REAL*8       SCAL__SNX(N__SNX)
      CHARACTER*4  NAME__SNX(N__SNX)*6, UNIT__SNX(N__SNX)*4
      INTEGER*4    STX__SNX, STY__SNX, STZ__SNX, VEX__SNX, VEY__SNX, VEZ__SNX, &
     &             XPL__SNX, YPL__SNX, UT1__SNX, &
     &             XPR__SNX, YPR__SNX, UTR__SNX, &
     &             PSI__SNX, EPS__SNX, &
     &             RAS__SNX, DCL__SNX, RAR__SNX, DCR__SNX, &
     &             AXI__SNX, NTX__SNX, NTY__SNX
      REAL*8       UTR__SNX__VALUE
      PARAMETER  ( UTR__SNX__VALUE = -1.D3*2.D0*PI__NUM_GLBC4/(OM__EAR_GLBC4*86400.0) )
!
! --- Constants for ADJST. They are related to SINEX listings
!
      PARAMETER  ( STX__SNX =  1 )
      PARAMETER  ( STY__SNX =  2 )
      PARAMETER  ( STZ__SNX =  3 )
      PARAMETER  ( VEX__SNX =  4 )
      PARAMETER  ( VEY__SNX =  5 )
      PARAMETER  ( VEZ__SNX =  6 )
      PARAMETER  ( XPL__SNX =  7 )
      PARAMETER  ( YPL__SNX =  8 )
      PARAMETER  ( UT1__SNX =  9 )
      PARAMETER  ( XPR__SNX = 10 )
      PARAMETER  ( YPR__SNX = 11 )
      PARAMETER  ( UTR__SNX = 12 )
      PARAMETER  ( PSI__SNX = 13 )
      PARAMETER  ( EPS__SNX = 14 )
      PARAMETER  ( RAS__SNX = 15 )
      PARAMETER  ( DCL__SNX = 16 )
      PARAMETER  ( RAR__SNX = 17 )
      PARAMETER  ( DCR__SNX = 18 )
      PARAMETER  ( AXI__SNX = 19 )
      PARAMETER  ( NTX__SNX = 20 )
      PARAMETER  ( NTY__SNX = 21 )
!
      DATA       NAME__SNX(STX__SNX) / 'STAX  ' /
      DATA       UNIT__SNX(STX__SNX) / 'm   '   /
      DATA       SCAL__SNX(STX__SNX) / 1.D0     /
!
      DATA       NAME__SNX(STY__SNX) / 'STAY  ' /
      DATA       UNIT__SNX(STY__SNX) / 'm   '   /
      DATA       SCAL__SNX(STY__SNX) / 1.D0     /
!
      DATA       NAME__SNX(STZ__SNX) / 'STAZ  ' /
      DATA       UNIT__SNX(STZ__SNX) / 'm   '   /
      DATA       SCAL__SNX(STZ__SNX) / 1.D0     /
!
      DATA       NAME__SNX(VEX__SNX) / 'VELX  ' /
      DATA       UNIT__SNX(VEX__SNX) / 'm/y '   /
      DATA       SCAL__SNX(VEX__SNX) / 1.D0     /
!
      DATA       NAME__SNX(VEY__SNX) / 'VELY  ' /
      DATA       UNIT__SNX(VEY__SNX) / 'm/y '   /
      DATA       SCAL__SNX(VEY__SNX) / 1.D0     /
!
      DATA       NAME__SNX(VEZ__SNX) / 'VELZ  ' /
      DATA       UNIT__SNX(VEZ__SNX) / 'm/y '   /
      DATA       SCAL__SNX(VEZ__SNX) / 1.D0     /
!
      DATA       NAME__SNX(XPL__SNX) / 'XPO   ' /
      DATA       UNIT__SNX(XPL__SNX) / 'mas '   /
      DATA       SCAL__SNX(XPL__SNX) / RAD__TO__MAS_GLBC4 /
!
      DATA       NAME__SNX(YPL__SNX) / 'YPO   ' /
      DATA       UNIT__SNX(YPL__SNX) / 'mas '   /
      DATA       SCAL__SNX(YPL__SNX) / RAD__TO__MAS_GLBC4 /
!
      DATA       NAME__SNX(UT1__SNX) / 'UT1   ' /
      DATA       UNIT__SNX(UT1__SNX) / 'ms  '   /
      DATA       SCAL__SNX(UT1__SNX) / 1.D3     /
!
      DATA       NAME__SNX(XPR__SNX) / 'XPOR  ' /
      DATA       UNIT__SNX(XPR__SNX) / 'masD'   /
      DATA       SCAL__SNX(XPR__SNX) / RAD__TO__MAS_GLBC4 /
!
      DATA       NAME__SNX(YPR__SNX) / 'YPOR  ' /
      DATA       UNIT__SNX(YPR__SNX) / 'masD'   /
      DATA       SCAL__SNX(YPR__SNX) / RAD__TO__MAS_GLBC4 /
!
      DATA       NAME__SNX(UTR__SNX) / 'LOD   ' /
      DATA       UNIT__SNX(UTR__SNX) / 'ms  '   /
      DATA       SCAL__SNX(UTR__SNX) / UTR__SNX__VALUE  /
!
      DATA       NAME__SNX(PSI__SNX) / 'NUT_LN' /
      DATA       UNIT__SNX(PSI__SNX) / 'mas '   /
      DATA       SCAL__SNX(PSI__SNX) / RAD__TO__MAS_GLBC4 /
!
      DATA       NAME__SNX(EPS__SNX) / 'NUT_OB' /
      DATA       UNIT__SNX(EPS__SNX) / 'mas '   /
      DATA       SCAL__SNX(EPS__SNX) / RAD__TO__MAS_GLBC4 /
!
      DATA       NAME__SNX(RAS__SNX) / 'RS_RA ' /
      DATA       UNIT__SNX(RAS__SNX) / 'rad '   /
      DATA       SCAL__SNX(RAS__SNX) /  1.0D0   /
!
      DATA       NAME__SNX(DCL__SNX) / 'RS_DE ' /
      DATA       UNIT__SNX(DCL__SNX) / 'rad '   /
      DATA       SCAL__SNX(DCL__SNX) /  1.0D0   /
!
      DATA       NAME__SNX(RAR__SNX) / 'RS_RAR' /
      DATA       UNIT__SNX(RAR__SNX) / 'r/yr'   /
      DATA       SCAL__SNX(RAR__SNX) /  1.0D0   /
!
      DATA       NAME__SNX(DCR__SNX) / 'RS_DER' /
      DATA       UNIT__SNX(DCR__SNX) / 'r/yr'   /
      DATA       SCAL__SNX(DCR__SNX) /  1.0D0   /
!
      DATA       NAME__SNX(AXI__SNX) / 'AXI_OF' /
      DATA       UNIT__SNX(AXI__SNX) / 'm   '   /
      DATA       SCAL__SNX(AXI__SNX) /  1.0D0   /
!
      DATA       NAME__SNX(NTX__SNX) / 'NUT_X ' /
      DATA       UNIT__SNX(NTX__SNX) / 'mas '   /
      DATA       SCAL__SNX(NTX__SNX) / RAD__TO__MAS_GLBC4 /
!
      DATA       NAME__SNX(NTY__SNX) / 'NUT_Y ' /
      DATA       UNIT__SNX(NTY__SNX) / 'mas '   /
      DATA       SCAL__SNX(NTY__SNX) / RAD__TO__MAS_GLBC4 /
!
! --- Constaints for ADJST. They keep definitions of constraint equations.
! --- They are used for SINEX output
!
      INTEGER*4  N__CNS
      PARAMETER  ( N__CNS = 78 )
      INTEGER*4  ATM_RAT__CNS, BLC_VAL__CNS, CLO_RAT__CNS, DCL_ORG__CNS, &
     &           EOP_UT1__CNS, EOP_XPL__CNS, EOP_YPL__CNS, EOR_UT1__CNS, &
     &           EOR_XPL__CNS, EOR_YPL__CNS, GRD_OFF__CNS, GRD_RAT__CNS, &
     &           NNR_POS__CNS, NNR_SRC__CNS, NNR_PRP__CNS, NNR_VEL__CNS, &
     &           NNT_POS__CNS, NNT_VEL__CNS, NUT_OFF__CNS, OAT_RAT__CNS, &
     &           OCL_RAT__CNS, RAS_ORG__CNS, SRC_COO__CNS, STA_ORG__CNS, &
     &           STA_PWC__CNS, STA_X__CNS,   STA_Y__CNS,   STA_Z__CNS,   &
     &           STA_U__CNS,   STA_E__CNS,   STA_N__CNS,   VEL_X__CNS,   &
     &           VEL_Y__CNS,   VEL_Z__CNS,   VEL_U__CNS,   VEL_E__CNS,   &
     &           VEL_N__CNS,   STA_TIE__CNS, UT1_RAT__CNS, VEL_DIR__CNS, &
     &           VEL_ORG__CNS, VEL_SET__CNS, VEL_TIE__CNS, VEL_VER__CNS, &
     &           XPL_RAT__CNS, YPL_RAT__CNS,                             &
     &           HPNT_1__CNS, HPNT_2__CNS, HPNT_3__CNS, HPNT_4__CNS,     &
     &           HPNT_5__CNS, HPNT_6__CNS, HPNT_7__CNS, HPNT_8__CNS,     &
     &           HPNR_1__CNS, HPNR_2__CNS, HPNR_3__CNS, HPNR_4__CNS,     &
     &           HPNR_5__CNS, HPNR_6__CNS, HPNR_7__CNS, HPNR_8__CNS,     &
     &           SPPS_1__CNS, SPPS_2__CNS, SPPS_3__CNS, SPPS_4__CNS,     &
     &           SPPS_5__CNS, SPPS_6__CNS, SPPS_7__CNS, SPPS_8__CNS,     &
     &           SPVL_1__CNS, SPVL_2__CNS, SPVL_3__CNS, SPVL_4__CNS,     &
     &           SPVL_5__CNS, SPVL_6__CNS, SPVL_7__CNS, SPVL_8__CNS
!
      PARAMETER  ( ATM_RAT__CNS =  1 )
      PARAMETER  ( BLC_VAL__CNS =  2 )
      PARAMETER  ( CLO_RAT__CNS =  3 )
      PARAMETER  ( DCL_ORG__CNS =  4 )
      PARAMETER  ( EOP_UT1__CNS =  5 )
      PARAMETER  ( EOP_XPL__CNS =  6 )
      PARAMETER  ( EOP_YPL__CNS =  7 )
      PARAMETER  ( EOR_UT1__CNS =  8 )
      PARAMETER  ( EOR_XPL__CNS =  9 )
      PARAMETER  ( EOR_YPL__CNS = 10 )
      PARAMETER  ( GRD_OFF__CNS = 11 )
      PARAMETER  ( GRD_RAT__CNS = 12 )
      PARAMETER  ( NNR_POS__CNS = 13 )
      PARAMETER  ( NNR_SRC__CNS = 14 )
      PARAMETER  ( NNR_PRP__CNS = 15 )
      PARAMETER  ( NNR_VEL__CNS = 16 )
      PARAMETER  ( NNT_POS__CNS = 17 )
      PARAMETER  ( NNT_VEL__CNS = 18 )
      PARAMETER  ( NUT_OFF__CNS = 19 )
      PARAMETER  ( OAT_RAT__CNS = 20 )
      PARAMETER  ( OCL_RAT__CNS = 21 )
      PARAMETER  ( RAS_ORG__CNS = 22 )
      PARAMETER  ( SRC_COO__CNS = 23 )
      PARAMETER  ( STA_E__CNS   = 24 )
      PARAMETER  ( STA_N__CNS   = 25 )
      PARAMETER  ( STA_ORG__CNS = 26 )
      PARAMETER  ( STA_PWC__CNS = 27 )
      PARAMETER  ( STA_TIE__CNS = 28 )
      PARAMETER  ( STA_U__CNS   = 29 )
      PARAMETER  ( STA_X__CNS   = 30 )
      PARAMETER  ( STA_Y__CNS   = 31 )
      PARAMETER  ( STA_Z__CNS   = 32 )
      PARAMETER  ( UT1_RAT__CNS = 33 )
      PARAMETER  ( VEL_DIR__CNS = 34 )
      PARAMETER  ( VEL_E__CNS   = 35 )
      PARAMETER  ( VEL_N__CNS   = 36 )
      PARAMETER  ( VEL_ORG__CNS = 37 )
      PARAMETER  ( VEL_SET__CNS = 38 )
      PARAMETER  ( VEL_TIE__CNS = 39 )
      PARAMETER  ( VEL_U__CNS   = 40 )
      PARAMETER  ( VEL_VER__CNS = 41 )
      PARAMETER  ( VEL_X__CNS   = 42 )
      PARAMETER  ( VEL_Y__CNS   = 43 )
      PARAMETER  ( VEL_Z__CNS   = 44 )
      PARAMETER  ( XPL_RAT__CNS = 45 )
      PARAMETER  ( YPL_RAT__CNS = 46 )
      PARAMETER  ( HPNT_1__CNS  = 47 )
      PARAMETER  ( HPNT_2__CNS  = 48 )
      PARAMETER  ( HPNT_3__CNS  = 49 )
      PARAMETER  ( HPNT_4__CNS  = 50 )
      PARAMETER  ( HPNT_5__CNS  = 51 )
      PARAMETER  ( HPNT_6__CNS  = 52 )
      PARAMETER  ( HPNT_7__CNS  = 53 )
      PARAMETER  ( HPNT_8__CNS  = 54 )
      PARAMETER  ( HPNR_1__CNS  = 55 )
      PARAMETER  ( HPNR_2__CNS  = 56 )
      PARAMETER  ( HPNR_3__CNS  = 57 )
      PARAMETER  ( HPNR_4__CNS  = 58 )
      PARAMETER  ( HPNR_5__CNS  = 59 )
      PARAMETER  ( HPNR_6__CNS  = 60 )
      PARAMETER  ( HPNR_7__CNS  = 61 )
      PARAMETER  ( HPNR_8__CNS  = 62 )
      PARAMETER  ( SPPS_1__CNS  = 63 )
      PARAMETER  ( SPPS_2__CNS  = 64 )
      PARAMETER  ( SPPS_3__CNS  = 65 )
      PARAMETER  ( SPPS_4__CNS  = 66 )
      PARAMETER  ( SPPS_5__CNS  = 67 )
      PARAMETER  ( SPPS_6__CNS  = 68 )
      PARAMETER  ( SPPS_7__CNS  = 69 )
      PARAMETER  ( SPPS_8__CNS  = 70 )
      PARAMETER  ( SPVL_1__CNS  = 71 )
      PARAMETER  ( SPVL_2__CNS  = 72 )
      PARAMETER  ( SPVL_3__CNS  = 73 )
      PARAMETER  ( SPVL_4__CNS  = 74 )
      PARAMETER  ( SPVL_5__CNS  = 75 )
      PARAMETER  ( SPVL_6__CNS  = 76 )
      PARAMETER  ( SPVL_7__CNS  = 77 )
      PARAMETER  ( SPVL_8__CNS  = 78 )
!
      CHARACTER  NAME__CNS(N__CNS)*8, UNIT__CNS(N__CNS)*4
      REAL*8     SCAL__CNS(N__CNS)
!
      DATA       NAME__CNS(ATM_RAT__CNS) / 'ATM_RATE' /
      DATA       UNIT__CNS(ATM_RAT__CNS) / 'd/l '     /
      DATA       SCAL__CNS(ATM_RAT__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(BLC_VAL__CNS) / 'BLC_VAL ' /
      DATA       UNIT__CNS(BLC_VAL__CNS) / 'sec '     /
      DATA       SCAL__CNS(BLC_VAL__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(CLO_RAT__CNS) / 'CLO_RATE' /
      DATA       UNIT__CNS(CLO_RAT__CNS) / 'd/l '     /
      DATA       SCAL__CNS(CLO_RAT__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(DCL_ORG__CNS) / 'DCL_ORG ' /
      DATA       UNIT__CNS(DCL_ORG__CNS) / 'rad '     /
      DATA       SCAL__CNS(DCL_ORG__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(EOP_XPL__CNS) / 'EOP_XPL ' /
      DATA       UNIT__CNS(EOP_XPL__CNS) / 'mas '     /
      DATA       SCAL__CNS(EOP_XPL__CNS) / RAD__TO__MAS_GLBC4 /
!
      DATA       NAME__CNS(EOP_YPL__CNS) / 'EOP_YPL ' /
      DATA       UNIT__CNS(EOP_YPL__CNS) / 'mas '     /
      DATA       SCAL__CNS(EOP_YPL__CNS) / RAD__TO__MAS_GLBC4 /
!
      DATA       NAME__CNS(EOP_UT1__CNS) / 'EOP_UT1 ' /
      DATA       UNIT__CNS(EOP_UT1__CNS) / 'ms  '     /
      DATA       SCAL__CNS(EOP_UT1__CNS) / 1.D3       /
!
      DATA       NAME__CNS(EOR_XPL__CNS) / 'EOR_XPL ' /
      DATA       UNIT__CNS(EOR_XPL__CNS) / 'masD'     /
      DATA       SCAL__CNS(EOR_XPL__CNS) / RAD__TO__MAS_GLBC4 /
!
      DATA       NAME__CNS(EOR_YPL__CNS) / 'EOR_YPL ' /
      DATA       UNIT__CNS(EOR_YPL__CNS) / 'masD'     /
      DATA       SCAL__CNS(EOR_YPL__CNS) / RAD__TO__MAS_GLBC4 /
!
      DATA       NAME__CNS(EOR_UT1__CNS) / 'EOR_UT1 ' /
      DATA       UNIT__CNS(EOR_UT1__CNS) / 'msD '     /
      DATA       SCAL__CNS(EOR_UT1__CNS) / UTR__SNX__VALUE /
!
      DATA       NAME__CNS(GRD_OFF__CNS) / 'GRAD_OFF' /
      DATA       UNIT__CNS(GRD_OFF__CNS) / 'mm  '     /
      DATA       SCAL__CNS(GRD_OFF__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(GRD_RAT__CNS) / 'GRAD_RAT' /
      DATA       UNIT__CNS(GRD_RAT__CNS) / 'mm/d'     /
      DATA       SCAL__CNS(GRD_RAT__CNS) /  1.0D0    /
!
      DATA       NAME__CNS(NNR_POS__CNS) / 'NNR_POS ' /
      DATA       UNIT__CNS(NNR_POS__CNS) / 'm   '     /
      DATA       SCAL__CNS(NNR_POS__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(NNR_SRC__CNS) / 'NNR_SRC ' /
      DATA       UNIT__CNS(NNR_SRC__CNS) / 'rad '     /
      DATA       SCAL__CNS(NNR_SRC__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(NNR_PRP__CNS) / 'NNR_SRC ' /
      DATA       UNIT__CNS(NNR_PRP__CNS) / 'r/yr'    /
      DATA       SCAL__CNS(NNR_PRP__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(NNR_VEL__CNS) / 'NNR_VEL ' /
      DATA       UNIT__CNS(NNR_VEL__CNS) / 'm/y '     /
      DATA       SCAL__CNS(NNR_VEL__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(NNT_POS__CNS) / 'NNT_POS ' /
      DATA       UNIT__CNS(NNT_POS__CNS) / 'm   '     /
      DATA       SCAL__CNS(NNT_POS__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(NNT_VEL__CNS) / 'NNT_VEL ' /
      DATA       UNIT__CNS(NNT_VEL__CNS) / 'm/y '     /
      DATA       SCAL__CNS(NNT_VEL__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(NUT_OFF__CNS) / 'NUT_OFF ' /
      DATA       UNIT__CNS(NUT_OFF__CNS) / 'mas '     /
      DATA       SCAL__CNS(NUT_OFF__CNS) /  RAD__TO__MAS_GLBC4 /
!
      DATA       NAME__CNS(OAT_RAT__CNS) / 'OAT_RAT ' /
      DATA       UNIT__CNS(OAT_RAT__CNS) / 'd/l '     /
      DATA       SCAL__CNS(OAT_RAT__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(OCL_RAT__CNS) / 'OCL_RAT ' /
      DATA       UNIT__CNS(OCL_RAT__CNS) / 'd/l '     /
      DATA       SCAL__CNS(OCL_RAT__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(RAS_ORG__CNS) / 'RAS_ORG ' /
      DATA       UNIT__CNS(RAS_ORG__CNS) / 'rad '     /
      DATA       SCAL__CNS(RAS_ORG__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(SRC_COO__CNS) / 'SRC_COO ' /
      DATA       UNIT__CNS(SRC_COO__CNS) / 'rad '     /
      DATA       SCAL__CNS(SRC_COO__CNS) / 1.0D0      /
!
      DATA       NAME__CNS(STA_ORG__CNS) / 'STA_ORG ' /
      DATA       UNIT__CNS(STA_ORG__CNS) / 'm   '     /
      DATA       SCAL__CNS(STA_ORG__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(STA_U__CNS)   / 'STA_U   ' /
      DATA       UNIT__CNS(STA_U__CNS)   / 'm   '     /
      DATA       SCAL__CNS(STA_U__CNS)   /  1.0D0     /
!
      DATA       NAME__CNS(STA_E__CNS)   / 'STA_E   ' /
      DATA       UNIT__CNS(STA_E__CNS)   / 'm   '     /
      DATA       SCAL__CNS(STA_E__CNS)   /  1.0D0     /
!
      DATA       NAME__CNS(STA_N__CNS)   / 'STA_N   ' /
      DATA       UNIT__CNS(STA_N__CNS)   / 'm   '     /
      DATA       SCAL__CNS(STA_N__CNS)   /  1.0D0     /
!
      DATA       NAME__CNS(STA_X__CNS)   / 'STA_X   ' /
      DATA       UNIT__CNS(STA_X__CNS)   / 'm   '     /
      DATA       SCAL__CNS(STA_X__CNS)   /  1.0D0     /
!
      DATA       NAME__CNS(STA_Y__CNS)   / 'STA_Y   ' /
      DATA       UNIT__CNS(STA_Y__CNS)   / 'm   '     /
      DATA       SCAL__CNS(STA_Y__CNS)   /  1.0D0     /
!
      DATA       NAME__CNS(STA_Z__CNS)   / 'STA_Z   ' /
      DATA       UNIT__CNS(STA_Z__CNS)   / 'm   '     /
      DATA       SCAL__CNS(STA_Z__CNS)   /  1.0D0     /
!
      DATA       NAME__CNS(STA_PWC__CNS) / 'STA_PWC ' /
      DATA       UNIT__CNS(STA_PWC__CNS) / 'm/yr'     /
      DATA       SCAL__CNS(STA_PWC__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(STA_TIE__CNS) / 'STA_TIE ' /
      DATA       UNIT__CNS(STA_TIE__CNS) / 'm   '     /
      DATA       SCAL__CNS(STA_TIE__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(UT1_RAT__CNS) / 'UT1_RATE' /
      DATA       UNIT__CNS(UT1_RAT__CNS) / 'msD '     /
      DATA       SCAL__CNS(UT1_RAT__CNS) /  UTR__SNX__VALUE /
!
      DATA       NAME__CNS(VEL_DIR__CNS) / 'VEL_DIR ' /
      DATA       UNIT__CNS(VEL_DIR__CNS) / 'm/y '     /
      DATA       SCAL__CNS(VEL_DIR__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(VEL_ORG__CNS) / 'VEL_ORG ' /
      DATA       UNIT__CNS(VEL_ORG__CNS) / 'm/y '     /
      DATA       SCAL__CNS(VEL_ORG__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(VEL_SET__CNS) / 'VEL_SET ' /
      DATA       UNIT__CNS(VEL_SET__CNS) / 'm/y '     /
      DATA       SCAL__CNS(VEL_SET__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(VEL_TIE__CNS) / 'VEL_TIE ' /
      DATA       UNIT__CNS(VEL_TIE__CNS) / 'm/y '     /
      DATA       SCAL__CNS(VEL_TIE__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(VEL_VER__CNS) / 'VEL_VER ' /
      DATA       UNIT__CNS(VEL_VER__CNS) / 'm/y '     /
      DATA       SCAL__CNS(VEL_VER__CNS) /  1.0D0     /
!
      DATA       NAME__CNS(VEL_U__CNS)   / 'VEL_U   ' /
      DATA       UNIT__CNS(VEL_U__CNS)   / 'm/y '     /
      DATA       SCAL__CNS(VEL_U__CNS)   /  1.0D0     /
!
      DATA       NAME__CNS(VEL_E__CNS)   / 'VEL_E   ' /
      DATA       UNIT__CNS(VEL_E__CNS)   / 'm/y '     /
      DATA       SCAL__CNS(VEL_E__CNS)   /  1.0D0     /
!
      DATA       NAME__CNS(VEL_N__CNS)   / 'VEL_N   ' /
      DATA       UNIT__CNS(VEL_N__CNS)   / 'm/y '     /
      DATA       SCAL__CNS(VEL_N__CNS)   /  1.0D0     /
!
      DATA       NAME__CNS(VEL_X__CNS)   / 'VEL_X   ' /
      DATA       UNIT__CNS(VEL_X__CNS)   / 'm/y '     /
      DATA       SCAL__CNS(VEL_X__CNS)   /  1.0D0     /
!
      DATA       NAME__CNS(VEL_Y__CNS)   / 'VEL_Y   ' /
      DATA       UNIT__CNS(VEL_Y__CNS)   / 'm/y '     /
      DATA       SCAL__CNS(VEL_Y__CNS)   /  1.0D0     /
!
      DATA       NAME__CNS(VEL_Z__CNS)   / 'VEL_Z   ' /
      DATA       UNIT__CNS(VEL_Z__CNS)   / 'm/y '     /
      DATA       SCAL__CNS(VEL_Z__CNS)   /  1.0D0     /
!
      DATA       NAME__CNS(XPL_RAT__CNS) / 'XPL_RAT ' /
      DATA       UNIT__CNS(XPL_RAT__CNS) / 'masD'     /
      DATA       SCAL__CNS(XPL_RAT__CNS) /  RAD__TO__MAS_GLBC4 /
!
      DATA       NAME__CNS(YPL_RAT__CNS) / 'YPL_RAT ' /
      DATA       UNIT__CNS(YPL_RAT__CNS) / 'masD'     /
      DATA       SCAL__CNS(YPL_RAT__CNS) /  RAD__TO__MAS_GLBC4 /
!
      DATA       NAME__CNS(HPNT_1__CNS)  / 'HPNT_001' /
      DATA       UNIT__CNS(HPNT_1__CNS)  / 'm   '     /
      DATA       SCAL__CNS(HPNT_1__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(HPNT_2__CNS)  / 'HPNT_002' /
      DATA       UNIT__CNS(HPNT_2__CNS)  / 'm   '     /
      DATA       SCAL__CNS(HPNT_2__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(HPNT_3__CNS)  / 'HPNT_003' /
      DATA       UNIT__CNS(HPNT_3__CNS)  / 'm   '     /
      DATA       SCAL__CNS(HPNT_3__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(HPNT_4__CNS)  / 'HPNT_004' /
      DATA       UNIT__CNS(HPNT_4__CNS)  / 'm   '     /
      DATA       SCAL__CNS(HPNT_4__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(HPNT_5__CNS)  / 'HPNT_005' /
      DATA       UNIT__CNS(HPNT_5__CNS)  / 'm   '     /
      DATA       SCAL__CNS(HPNT_5__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(HPNT_6__CNS)  / 'HPNT_006' /
      DATA       UNIT__CNS(HPNT_6__CNS)  / 'm   '     /
      DATA       SCAL__CNS(HPNT_6__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(HPNT_7__CNS)  / 'HPNT_007' /
      DATA       UNIT__CNS(HPNT_7__CNS)  / 'm   '     /
      DATA       SCAL__CNS(HPNT_7__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(HPNT_8__CNS)  / 'HPNT_008' /
      DATA       UNIT__CNS(HPNT_8__CNS)  / 'm   '     /
      DATA       SCAL__CNS(HPNT_8__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(HPNR_1__CNS)  / 'HPNR_001' /
      DATA       UNIT__CNS(HPNR_1__CNS)  / 'm   '     /
      DATA       SCAL__CNS(HPNR_1__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(HPNR_2__CNS)  / 'HPNR_002' /
      DATA       UNIT__CNS(HPNR_2__CNS)  / 'm   '     /
      DATA       SCAL__CNS(HPNR_2__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(HPNR_3__CNS)  / 'HPNR_003' /
      DATA       UNIT__CNS(HPNR_3__CNS)  / 'm   '     /
      DATA       SCAL__CNS(HPNR_3__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(HPNR_4__CNS)  / 'HPNR_004' /
      DATA       UNIT__CNS(HPNR_4__CNS)  / 'm   '     /
      DATA       SCAL__CNS(HPNR_4__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(HPNR_5__CNS)  / 'HPNR_005' /
      DATA       UNIT__CNS(HPNR_5__CNS)  / 'm   '     /
      DATA       SCAL__CNS(HPNR_5__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(HPNR_6__CNS)  / 'HPNR_006' /
      DATA       UNIT__CNS(HPNR_6__CNS)  / 'm   '     /
      DATA       SCAL__CNS(HPNR_6__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(HPNR_7__CNS)  / 'HPNR_007' /
      DATA       UNIT__CNS(HPNR_7__CNS)  / 'm   '     /
      DATA       SCAL__CNS(HPNR_7__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(HPNR_8__CNS)  / 'HPNR_008' /
      DATA       UNIT__CNS(HPNR_8__CNS)  / 'm   '     /
      DATA       SCAL__CNS(HPNR_8__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(SPPS_1__CNS)  / 'SPPS_001' /
      DATA       UNIT__CNS(SPPS_1__CNS)  / 'm   '     /
      DATA       SCAL__CNS(SPPS_1__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(SPPS_2__CNS)  / 'SPPS_002' /
      DATA       UNIT__CNS(SPPS_2__CNS)  / 'm   '     /
      DATA       SCAL__CNS(SPPS_2__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(SPPS_3__CNS)  / 'SPPS_003' /
      DATA       UNIT__CNS(SPPS_3__CNS)  / 'm   '     /
      DATA       SCAL__CNS(SPPS_3__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(SPPS_4__CNS)  / 'SPPS_004' /
      DATA       UNIT__CNS(SPPS_4__CNS)  / 'm   '     /
      DATA       SCAL__CNS(SPPS_4__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(SPPS_5__CNS)  / 'SPPS_005' /
      DATA       UNIT__CNS(SPPS_5__CNS)  / 'm   '     /
      DATA       SCAL__CNS(SPPS_5__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(SPPS_6__CNS)  / 'SPPS_006' /
      DATA       UNIT__CNS(SPPS_6__CNS)  / 'm   '     /
      DATA       SCAL__CNS(SPPS_6__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(SPPS_7__CNS)  / 'SPPS_007' /
      DATA       UNIT__CNS(SPPS_7__CNS)  / 'm   '     /
      DATA       SCAL__CNS(SPPS_7__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(SPPS_8__CNS)  / 'SPPS_008' /
      DATA       UNIT__CNS(SPPS_8__CNS)  / 'm   '     /
      DATA       SCAL__CNS(SPPS_8__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(SPVL_1__CNS)  / 'SPVL_001' /
      DATA       UNIT__CNS(SPVL_1__CNS)  / 'm   '     /
      DATA       SCAL__CNS(SPVL_1__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(SPVL_2__CNS)  / 'SPVL_002' /
      DATA       UNIT__CNS(SPVL_2__CNS)  / 'm   '     /
      DATA       SCAL__CNS(SPVL_2__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(SPVL_3__CNS)  / 'SPVL_003' /
      DATA       UNIT__CNS(SPVL_3__CNS)  / 'm   '     /
      DATA       SCAL__CNS(SPVL_3__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(SPVL_4__CNS)  / 'SPVL_004' /
      DATA       UNIT__CNS(SPVL_4__CNS)  / 'm   '     /
      DATA       SCAL__CNS(SPVL_4__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(SPVL_5__CNS)  / 'SPVL_005' /
      DATA       UNIT__CNS(SPVL_5__CNS)  / 'm   '     /
      DATA       SCAL__CNS(SPVL_5__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(SPVL_6__CNS)  / 'SPVL_006' /
      DATA       UNIT__CNS(SPVL_6__CNS)  / 'm   '     /
      DATA       SCAL__CNS(SPVL_6__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(SPVL_7__CNS)  / 'SPVL_007' /
      DATA       UNIT__CNS(SPVL_7__CNS)  / 'm   '     /
      DATA       SCAL__CNS(SPVL_7__CNS)  /  1.0D0     /
!
      DATA       NAME__CNS(SPVL_8__CNS)  / 'SPVL_008' /
      DATA       UNIT__CNS(SPVL_8__CNS)  / 'm   '     /
      DATA       SCAL__CNS(SPVL_8__CNS)  /  1.0D0     /
!
      INTEGER*4  L_HPE, L_SPE, L_BSP
      INTEGER*8 :: ADR_HPE, ADR_SPE, ADR_BSP
!
      INTEGER*4    JDT__UNDF, JDT__AVAL
      PARAMETER  ( JDT__UNDF =  -1037984562 ) ! Status: undefined
      PARAMETER  ( JDT__AVAL =  1 ) ! Status: available
!
      COMMON / GLBC4 / BEGMARK_GLBC4_I4, &
     &        SITDIF(3,MAX_ARC_STA),  STRDIF(2,MAX_ARC_SRC), &
     &        NVSITEC(3,MAX_ARC_STA), NVSITEV(3,MAX_ARC_STA), &
     &        NVSTARC(2,MAX_ARC_SRC), NVNUT(2,6), NVNUTOP(2,6), &
     &        DPRIN(4), DDECA(4), DANNU(4), DSEMA(4), D122D(4), DSEMM(4), DNUT(5), DDPSI(5), DDEPS(5), DPREC, &
     &        UT1PTV(MAX_FLYBY_EOP_VALUES), WOBXXV(MAX_FLYBY_EOP_VALUES), &
     &        WOBYYV(MAX_FLYBY_EOP_VALUES), WBXSIG(MAX_FLYBY_EOP_VALUES), &
     &        WBYSIG(MAX_FLYBY_EOP_VALUES), UT1SIG(MAX_FLYBY_EOP_VALUES), &
     &        WXYCOR(MAX_FLYBY_EOP_VALUES), WXUCOR(MAX_FLYBY_EOP_VALUES), &
     &        WYUCOR(MAX_FLYBY_EOP_VALUES), &
     &        KSTAP, KSOUC, KNUTS, KNUTD, KEROT, KSVEL, KSTAM, KECC, KMGR, KMET, &
     &        STASUB, SRCSUB, NUTSRS, NUTDLY, EOPDLY, VELSUB, NVPREC, PLTMOD1, PLTMOD2, &
     &        STAXYZ_CNST(3), STAXYZ_CNSB(STA_BIT_WORDS), STAXYZ_CNFL, &
     &        STAUEN_CNST(3), STAUEN_CNSB(STA_BIT_WORDS), STAUEN_CNFL, &
     &        VELXYZ_CNST(3), VELXYZ_CNSB(STA_BIT_WORDS), VELXYZ_CNFL, &
     &        VELUEN_CNST(3), VELUEN_CNSB(STA_BIT_WORDS), VELUEN_CNFL, &
     &        DEFNUVCOV, PLATE_FACT, UT1INV(3), WOBINV(3), &
!
     &        FIXED_STA(4), FLYBY_INTERP, PWCNUM(MAX_STA), PWCEP(MAX_PWC_EPS), &
     &        PWCSITES(MAX_PWC_SITES), PWC_INTRVL, EVINT, EVSTART, &
!
     &        NUM_PART, PART_APPLIED, STA_WT_FIL, &
     &        STAMOD1, STAMOD2, VELMOD1, VELMOD2, SRCMOD1, &
     &        SRCMOD2, STAREF, SRCREF, NUM_SDC_UT1, &
     &        NUM_SDC_XY, SDC_ARG(6,MAX_SDC), SDC_VAL(2,MAX_SDC), &
     &        KHFEOP, NUM_SDE_UT1, &
     &        NUM_SDE_XY, SDE_ARG(6,MAX_SDC), SDE_VAL(2,MAX_SDC), &
     &        HFEOPF, PLCALF, SITPLD(MAX_ARC_STA), KPLODCAL, &
     &        KIONO_CORR, REFPRES(MAX_ARC_STA), PLTMOD, &
     &        STAPOSEPOCH, STAPOSNUM, NUVCOVFLG(STA_BIT_WORDS), &
     &        RESOUTFILE, KAXOP, AXOSUB, AXDIF(MAX_ARC_STA), &
     &        MINIMIZE_SIGS, SUBTRACT_ARC, LNSIG(5), OBSIG(5), &
     &        LNOBCOR(5), RCOND, KBEEP, PART_ARRAY, &
     &        KCENTERMASS, SITPL_FIL, SOURCE_WEIGHT_FILE, &
     &        SOURCE_WEIGHTS, KELDEP_NOISE, ELDEP_FILE, MIN_ET(4), &
     &        FAST_MODE,     FAST_DBG,     FAST_COV, &
     &        FAST_MODE_GLO, FAST_DBG_GLO, FAST_COV_GLO, &
     &        REWAY_ITCOU,   REWAY_FLODEL, REWAY_FLORATE, REWAY_CEIDEL, &
     &        REWAY_CEIRATE, REWAY_CHITOL, REWAY_MAXIT,   REWAY_VERBOSE, &
     &        REWAY_TYPE,    REWAY_NEWUPD,  REWAY_FALLBACK, &
     &        GAMB_F_X_BAND, GAMB_F_S_BAND, &
     &        GAMB_F_ION,   GAMB_F_PREUSE, GAMB_F_SAVE, GAMB_CUTOFF, &
     &        GAMB_MINOBS,   GAMB_SPACING_CONST, GAMB_IT, GAMB_BATCH, &
     &        IONO_VERBOSE, IONO_SEEK, SETFL_MDEG, DEF_ATMSPAN, DEF_CLOSPAN, &
     &        QUALCODE_GOOD_LIM, SOLVE_EDITOR, OLD_SOLVE_PS_VIEWER, SOLVE_EMULATION, &
     &        COND_WARNING, SEG_OUTPUT, &
     &        ELIM_MOD, ELIM_CNF, ELIM_THR, ELIM_CUT, ELIM_MSR, &
     &        ELIM_VRB, ELIM_TYP, ELIM_UPD, MAPPED_EOP_OUTPUT,  MAPPED_EOP_TIME, &
     &        ESTIMATE_NUTATION_FIRST, ESTIMATE_UT1_RATE_FIRST, &
     &        ESTIMATE_STATION_FIRST, ESTIMATE_EOP_FIRST, &
     &        SAVING_RATE, RCOND_CGM, ELIM_AMB, ELIM_ION, &
     &        PAMB_VRB, PAMB_PLOT_TYPE, PAMB_PLOT_BAND, PAMB_PARU_FILE, &
     &        PAMB_PSL_TYPE, PAMB_XGRLIM, PAMB_SGRLIM, PAMB_XPHLIM, PAMB_SPHLIM, &
     &        PAMB_DEFRG, PAMB_ARFMS, PAMB_RLIM3, PAMB_FRZTR, PAMB_SPLSPAN, &
     &        PAMB_SPL_CNST, PAMB_INIWEI, PAMB_ARFFLO, PAMB_MSC, PAMB_ARFTYPE, &
     &        PAMB_PSTA, PAMB_FSTA, PAMB_PLOTINI, PAMB_PLOTFIN, &
     &        LIN_STA_SIGMA,  BAS_CLK_SIGMA, SRC_COO_SIGMA,  SRC_PRP_SIGMA, &
     &        NNT_POS_SIGMA,  NNR_POS_SIGMA,  NNT_VEL_SIGMA, NNR_VEL_SIGMA, &
     &        STA_WEA_SIGMA,  VEL_WEA_SIGMA,  VEL_DIR_SIGMA, VEL_CMP_SIGMA, &
     &        VEL_SET_SIGMA,  STA_ORG_SIGMA,  VEL_ORG_SIGMA, STA_TIE_SIGMA, &
     &        VEL_TIE_SIGMA,  RAS_ORG_SIGMA,  DCL_ORG_SIGMA, NNR_SRC_SIGMA, &
     &        NNR_PRP_SIGMA,  NUT_CMP_SIGMA,  VEL_VER_SIGMA, SNX_WOB_SIGMA, &
     &        SNX_UT1_SIGMA,  SNX_NUT_SIGMA,  SNX_POS_SIGMA, SNX_VEL_SIGMA, &
     &        CRES_STYLE,   CNPLT_SUPR_PRE98, CNPLT_SUPR_SIMPLE, &
     &        CNPLT_SHOW_UNRC, CNPLT_SHOW_CBAD, CNPLT_PHASE_S, &
     &        SUPMET_BAT, SNGCHK_ACTION, SNGCHK_SOUMIN, SNGCHK_STAMIN, &
     &        SNGCHK_BASMIN, TRAIN, GLO_PARLIM, INC_PARLIM, &
     &        SORT_SOU, SORT_STA, G_WARNING, MEMFAULT_REP, &
     &        MDLPL_IPS_PAG,   MDLPL_IPC_PAG, &
     &        MDLPL_FL_EOPMOD, MDLPL_FL_CLF, MDLPL_FL_FRAME, WEB_DIR, &
     &        COR_FLAG, COR_GG_FLAG, COR_GL_FLAG, &
     &        COR_LL_FLAG, COR_CL_FLAG, COROUT_FORM, &
     &        COR_GG_INCFIL, COR_GG_EXCFIL, COR_GL_INCFIL, COR_GL_EXCFIL, &
     &        COR_LL_INCFIL, COR_LL_EXCFIL, COR_CL_INCFIL, COR_CL_EXCFIL, &
     &        COR_CL_INCSES, COR_CL_EXCSES, &
     &        CONTROL_FILE, NORATE_FLAG, EQUMEM_FLAG, APRIORI_ZENDEL, SUP_CONFIG, &
!
     &        FL_SINEX_MAKE, FL_SINEX_GLO,  FL_SINEX_LOC, FL_SINEX_SEG, FL_SINEX_EST,  FL_SINEX_COV, &
     &        FL_SINEX_CNS,  FL_SINEX_DCM,  FL_NRD_TABLE, FL_CHI_TABLE, SINEX_OUTFILE, SINEX_INCLUDE, SINEX_EXCLUDE, &
     &        SINEX_ACKFIL,  SINEX_COMFIL,  SINEX_REALNAME, SINEX_VERS, &
!
     &        JDATE_ALL_BEG, JDATE_ALL_MID, JDATE_ALL_END, JDATE_STA_BEG, JDATE_STA_MID, JDATE_STA_END, STATUS_JDATE, &
     &        STAT_NUMOBS, STAT_NUMUNK, STAT_SQUOC,  STAT_SQURES, &
     &        STAT_VARFAC, STAT_WRMS, NNT_POS_RTP, NNR_POS_RTP, NNT_VEL_RTP, &
     &        NNR_VEL_RTP, NNR_SRC_RTP, NNR_PRP_RTP, &
     &        N_POSVAR, POSVAR_MOD, POSVAR_INT, POSVAR_USE, POSVAR_FIL, FL_WARN_POSVAR, POSVAR_RD_AREA, &
     &        N_PSVHAR, N_PSVSTA, SRC_LISTING_STYLE, STA_LISTING_STYLE, BAS_LISTING_STYLE, SEG_LISTING_STYLE, &
!
     &        STS_NAMSIT, ADR_NAMSIT, LEN_NAMSIT, STS_HARVAL, ADR_HARVAL, LEN_HARVAL, &
     &        STS_HARDSP, ADR_HARDSP, LEN_HARDSP, STS_STACOO, ADR_STACOO, LEN_STACOO, &
     &        STS_BDSFIL, ADR_BDSFIL, LEN_BDSFIL, STS_BDSSAM, ADR_BDSSAM, LEN_BDSSAM, &
     &        STS_BDSFMJ, ADR_BDSFMJ, LEN_BDSFMJ, STS_BDSFSC, ADR_BDSFSC, LEN_BDSFSC, &
     &        STS_BDSLMJ, ADR_BDSLMJ, LEN_BDSLMJ, STS_BDSLSC, ADR_BDSLSC, LEN_BDSLSC, &
     &        STS_BDSNSA, ADR_BDSNSA, LEN_BDSNSA, TIM_PSVFIL, BDS_ENDIAN, BDS_FLOAT, &
!
     &        VTD_CONF_GLB, VTD_CONF_SES, VTD_ADR, VTD_STATUS, FL_VTD_GLB, FL_VTD_SES, MJD_EHEO_REF, TAI_EHEO_REF, L_HPE, &
     &        L_SPE, L_BSP, ADR_HPE, ADR_SPE, ADR_BSP, L_HEO, ADR_HEO, STAT_HEO, FINAM_HEO, NAME_HEO, HEO_EPOCH_SEC, &
     &        L_AEM, ADR_AEM, GRAD_AUG2003_BUG, FRINGE_ROOT_DIR, STASUB2_CHR, VELSUB2_CHR, PLTSUB2_CHR, ECCSUB_CHR,  &
     &        MGRSUB_CHR,  METSUB_CHR,  PLATE_SCL, L_EHEC, ADR_EHEC, L_EERM, ADR_EERM, L_MERM, ADR_MERM, STAT_MERM, FINAM_MERM, &
     &        FL_NOFLYBY, BATCH_CNF_FINAM, L_EHEO, ADR_EHEO, ADR_EHES, SOU_ADM_FLAG, SOU_ADM_CNS, SUPPRESS_FILE, EDC_USE, &
     &        EDC_PAR, EDC_DIR, TPD_FLAG, TPD_DIR, N_TPD_INIT, TPD_INIT_LIST, RWT_EL_USE, RWT_SRC_USE, RWT_EL_GLB, &
     &        RWT_STA_EL_FIL, RWT_SRC_FIL, TRP_DIR, TRP_USE, N_FIL_TRP, ADR_TRP_FIL_BUF, STS_TRP_FIL, ADR_TRP_SES_BUF, &
     &        STS_TRP_SES, ADR_TRP, STS_TRP, NUT_USE_CODE, ATD_USE, ATD_FIL, ATD_MEA_USE, ATD_MEA_FIL, ATD_ADR, ATD_STS, AOC_FIL, DTEC_FIL, DTEC_USE, TEC_BIAS, TEC_SCAL, &
     &        TEC_NOISE_FILE, SEED_INIT_I4, FL_NO_IONO_DEL, GLO_SNR_MIN_X, GLO_SNR_MIN_S, SNR_MIN_X, SNR_MIN_S, IONOV_DIR, IONOV_USE, IONO_ERR_FCT, ASM_NAM, ASM_STA_FIL, &
     &        FRTYPFIT, FL_EQUAL_EFF_FREQ, EDIT_FIL, ADDW_FIL, ADDW_SCL, SESS_REWEI_SCALE, SESS_REWEI_QUADR, &
     &        GIM_INFO_DIR, GIM_DTEC_DIR, GIM_ADDW_DIR, GIM_DEL_DIR, GIM_NOI_DIR, BCL_FIL, BRK_FIL, GIM_TIM_STEP, GIM_SCALE, GIM_MODE, GIM_DEG, GIM_SEED, GIM_VERB, GIM_COLLECT_INFO, GIM_EST, GIM_RGR, GIM_WRI, &
     &        DTEC_SBA_USE, DTEC_ERR_SCL, EXT_ERR_FIL, VCAT_CONF_FILE, VCAT_REPO, ADDW_USE, GLBC4_FILLER(LEN_GLBC4_FILLER), IFGLB4(21), ENDMARK_GLBC4_I4 ! Always keep IFGBL4 just before filler. 
!
      REAL*8         SITDIF,  STRDIF, &
     &               NVSITEC, NVSITEV, NVSTARC, NVNUT, NVNUTOP, &
     &               DPRIN,   DDECA,  DANNU, &
     &               DSEMA,   D122D,  DSEMM, DPREC, &
     &               DNUT,    DDPSI,  DDEPS, &
     &               UT1PTV,  WOBXXV, WOBYYV, UT1INV, WOBINV, &
     &               WBXSIG,  WBYSIG, UT1SIG, &
     &               WXYCOR,  WXUCOR, WYUCOR, NVPREC, &
     &               REWAY_FLODEL, REWAY_FLORATE, &
     &               REWAY_CEIDEL, REWAY_CEIRATE, REWAY_CHITOL, &
     &               GAMB_CUTOFF, GAMB_SPACING_CONST, &
     &               ELIM_THR, ELIM_CUT, ELIM_MSR, &
     &               COND_WARNING, MAPPED_EOP_TIME
      LOGICAL*2      KSTAP,   KSOUC,  KNUTS,  KNUTD,  KEROT, KSVEL, &
     &               KSTAM,   KAXOP,  KECC,   KMGR,   KMET
      INTEGER*2      STASUB(NAME_WORDS), SRCSUB(NAME_WORDS)
      INTEGER*2      NUTSRS(NAME_WORDS), NUTDLY(NAME_WORDS)
      INTEGER*2      EOPDLY(NAME_WORDS), VELSUB(NAME_WORDS)
      INTEGER*2      HFEOPF(NAME_WORDS), PLCALF(NAME_WORDS)
      INTEGER*2      PLTMOD(NAME_WORDS), AXOSUB(NAME_WORDS)
!
      CHARACTER      STASUB_CHR*(2*NAME_WORDS), SRCSUB_CHR*(2*NAME_WORDS)
      CHARACTER      NUTSRS_CHR*(2*NAME_WORDS), NUTDLY_CHR*(2*NAME_WORDS)
      CHARACTER      EOPDLY_CHR*(2*NAME_WORDS), VELSUB_CHR*(2*NAME_WORDS)
      CHARACTER      HFEOPF_CHR*(2*NAME_WORDS), PLCALF_CHR*(2*NAME_WORDS)
      CHARACTER      PLTMOD_CHR*(2*NAME_WORDS), AXOSUB_CHR*(2*NAME_WORDS)
!
      EQUIVALENCE  ( STASUB, STASUB_CHR )
      EQUIVALENCE  ( SRCSUB, SRCSUB_CHR )
      EQUIVALENCE  ( NUTSRS, NUTSRS_CHR )
      EQUIVALENCE  ( NUTDLY, NUTDLY_CHR )
      EQUIVALENCE  ( EOPDLY, EOPDLY_CHR )
      EQUIVALENCE  ( VELSUB, VELSUB_CHR )
      EQUIVALENCE  ( HFEOPF, HFEOPF_CHR )
      EQUIVALENCE  ( PLCALF, PLCALF_CHR )
      EQUIVALENCE  ( PLTMOD, PLTMOD_CHR )
      EQUIVALENCE  ( AXOSUB, AXOSUB_CHR )
!
      INTEGER*2      FIXED_STA, PWCNUM,  REWAY_ITCOU, REWAY_MAXIT, &
     &               REWAY_FALLBACK, REWAY_NEWUPD
      LOGICAL*2      REWAY_VERBOSE
      CHARACTER      SOURCE_WEIGHT_FILE*(NAME_BYTES), &
     &               ELDEP_FILE*(NAME_BYTES), TEC_NOISE_FILE*128
      CHARACTER*8    PWCSITES
      CHARACTER      REWAY_TYPE*2, ELIM_TYP*2
      REAL*8         PWCEP
      REAL*8         GLBC4EQ(JGLBC4_BLOCKS*32)
      EQUIVALENCE    (GLBC4EQ(1),SITDIF(1,1))
      REAL*8         STAXYZ_CNST, STAUEN_CNST, VELXYZ_CNST, VELUEN_CNST
      INTEGER*2      STAXYZ_CNSB, STAUEN_CNSB, &
     &               VELXYZ_CNSB, VELUEN_CNSB
      LOGICAL*2      STAXYZ_CNFL, STAUEN_CNFL, VELXYZ_CNFL, VELUEN_CNFL
      INTEGER*2      NUM_PART, PART_APPLIED
      INTEGER*2      IFGLB4, FLYBY_INTERP, PWC_INTRVL, EVINT, EVSTART
      CHARACTER*8    STAREF, SRCREF, PLTMOD1, PLTMOD2
      CHARACTER*8    STAMOD1, STAMOD2, VELMOD1, VELMOD2
      CHARACTER*8    SRCMOD1, SRCMOD2, PART_ARRAY(112)
      INTEGER*2      NUM_SDC_UT1, NUM_SDC_XY, SDC_ARG
      INTEGER*2      NUM_SDE_UT1, NUM_SDE_XY, SDE_ARG
      REAL*8         SDC_VAL, SDE_VAL, SITPLD, REFPRES, STAPOSEPOCH
      REAL*8         PLATE_FACT
      INTEGER*2      KHFEOP, STAPOSNUM, DEFNUVCOV, NUVCOVFLG
      LOGICAL*2      KPLODCAL, KIONO_CORR, MINIMIZE_SIGS, SUBTRACT_ARC
      CHARACTER*80   RESOUTFILE
      REAL*8         AXDIF, LNSIG, OBSIG, LNOBCOR, RCOND, rcond_cgm, &
     &               MIN_ET
      LOGICAL*2      KBEEP, KCENTERMASS, KELDEP_NOISE
      CHARACTER*48   STA_WT_FIL
      CHARACTER*20   SITPL_FIL
      CHARACTER*2    SOURCE_WEIGHTS
      INTEGER*4      FAST_MODE,     FAST_DBG,      FAST_COV
      INTEGER*4      FAST_MODE_GLO, FAST_DBG_GLO,  FAST_COV_GLO
      INTEGER*4      GAMB_MINOBS,   GAMB_IT
      LOGICAL*4      GAMB_F_X_BAND, GAMB_F_S_BAND, &
     &               GAMB_F_ION,    GAMB_F_PREUSE, GAMB_F_SAVE, &
     &               GAMB_BATCH
      LOGICAL*4      ELIM_MOD, ELIM_CNF, ELIM_AMB, ELIM_ION
      LOGICAL*4      IONO_VERBOSE
      INTEGER*4      IONO_SEEK
      INTEGER*4      SETFL_MDEG
      REAL*8         DEF_ATMSPAN, DEF_CLOSPAN
      INTEGER*4      QUALCODE_GOOD_LIM, SOLVE_EMULATION
      INTEGER*4      ELIM_VRB, ELIM_UPD
      INTEGER*4      SAVING_RATE
      INTEGER*4      PAMB_VRB, PAMB_PLOT_TYPE, PAMB_PLOT_BAND, &
     &               PAMB_PSL_TYPE
      INTEGER*4      SNGCHK_ACTION, SNGCHK_SOUMIN, &
     &               SNGCHK_STAMIN, SNGCHK_BASMIN
!
      LOGICAL*4      TRAIN
      INTEGER*4      GLO_PARLIM, INC_PARLIM, &
     &               SORT_SOU,  SORT_STA
      CHARACTER      SOLVE_EDITOR*16, OLD_SOLVE_PS_VIEWER*16
      CHARACTER      PAMB_PARU_FILE*32
      LOGICAL*4      NORATE_FLAG, EQUMEM_FLAG
      LOGICAL*2      FL_NO_IONO_DEL, FL_EQUAL_EFF_FREQ
!
      INTEGER*4  SRC_LISTING_STYLE, STA_LISTING_STYLE, BAS_LISTING_STYLE, &
     &           SEG_LISTING_STYLE
!
      REAL*8      LIN_STA_SIGMA,  BAS_CLK_SIGMA, &
     &            SRC_COO_SIGMA,  SRC_PRP_SIGMA,  &
     &            NNT_POS_SIGMA,  NNR_POS_SIGMA, &
     &            NNT_VEL_SIGMA,  NNR_VEL_SIGMA, &
     &            STA_WEA_SIGMA,  VEL_WEA_SIGMA, &
     &            VEL_DIR_SIGMA,  VEL_CMP_SIGMA, &
     &            VEL_SET_SIGMA,  STA_ORG_SIGMA, &
     &            VEL_ORG_SIGMA,  STA_TIE_SIGMA, &
     &            VEL_TIE_SIGMA,  RAS_ORG_SIGMA, &
     &            DCL_ORG_SIGMA,  NNR_SRC_SIGMA, &
     &            NNR_PRP_SIGMA,  NUT_CMP_SIGMA, &
     &            VEL_VER_SIGMA,  SNX_WOB_SIGMA, &
     &            SNX_UT1_SIGMA,  SNX_NUT_SIGMA, &
     &            SNX_POS_SIGMA,  SNX_VEL_SIGMA
      REAL*8      PAMB_XGRLIM, PAMB_SGRLIM, PAMB_XPHLIM, PAMB_SPHLIM, &
     &            PAMB_DEFRG, PAMB_ARFMS, PAMB_RLIM3, PAMB_FRZTR, &
     &            PAMB_SPLSPAN, PAMB_SPL_CNST, PAMB_INIWEI, PAMB_ARFFLO, &
     &            ERR_FUDGE_FACTOR
      INTEGER*4   PAMB_FSTA, PAMB_ARFTYPE, PAMB_PLOTINI, PAMB_PLOTFIN
      INTEGER*4   L_EERM, L_MERM, STAT_MERM
      INTEGER*8 :: ADR_EERM, ADR_MERM
      CHARACTER   FINAM_MERM*128
!
! --- Variables for POSVAR (site position variations) option
!
      INTEGER*4  MAX__POSVAR
      PARAMETER ( MAX__POSVAR = 8 ) ! NB: should be the same as M__POSVAR from bindisp.i
      INTEGER*4  N_PSVHAR(MAX__POSVAR)   ! Number of harmonics
      INTEGER*4  N_PSVSTA(MAX__POSVAR)   ! Number of sites
      LOGICAL*4  FL_WARN_POSVAR(MAX__POSVAR) ! Flag: whether to issue a warning
!                                            ! for a missing station
      REAL*8     POSVAR_RD_AREA(MAX__POSVAR) ! Radius of the applicability area
!
! --- Variables below defines status, address of allocated memory and
! --- length of the memory for a dynamic data structure.
!
! --- Site name table:  C*1 ::  8*N_PSVSTA
!
      INTEGER*4        STS_NAMSIT(MAX__POSVAR)
      INTEGER*8 :: ADR_NAMSIT(MAX__POSVAR)
      INTEGER*8        LEN_NAMSIT(MAX__POSVAR)
!
! --- Harnmic definition table (phase, frequency and accelaration)
! --- R*8 :: 3*PSVHAR
!
      INTEGER*4        STS_HARVAL(MAX__POSVAR)
      INTEGER*8 :: ADR_HARVAL(MAX__POSVAR)
      INTEGER*8        LEN_HARVAL(MAX__POSVAR)
!
! --- Harmonic displacement table:  R*8 :: N_PSVHAR*N_PSVSTA
!
      INTEGER*4        STS_HARDSP(MAX__POSVAR)
      INTEGER*8 :: ADR_HARDSP(MAX__POSVAR)
      INTEGER*8        LEN_HARDSP(MAX__POSVAR)
!
! --- Station coordinates table: R8 :: 3*N_PSVSTA
!
      INTEGER*4        STS_STACOO(MAX__POSVAR)
      INTEGER*8 :: ADR_STACOO(MAX__POSVAR)
      INTEGER*8        LEN_STACOO(MAX__POSVAR)
!
! --- BINDISP file name table: C1 :: 128*N_PSVSTA
!
      INTEGER*4        STS_BDSFIL(MAX__POSVAR)
      INTEGER*8 :: ADR_BDSFIL(MAX__POSVAR)
      INTEGER*8        LEN_BDSFIL(MAX__POSVAR)
!
! --- BINDISP sampling interval table: R8 :: N_PSVSTA
!
      INTEGER*4        STS_BDSSAM(MAX__POSVAR)
      INTEGER*8 :: ADR_BDSSAM(MAX__POSVAR)
      INTEGER*8        LEN_BDSSAM(MAX__POSVAR)
!
! --- BINDISP MJD for first epochs: I4 :: N_PSVSTA
!
      INTEGER*4        STS_BDSFMJ(MAX__POSVAR)
      INTEGER*8 :: ADR_BDSFMJ(MAX__POSVAR)
      INTEGER*8        LEN_BDSFMJ(MAX__POSVAR)
!
! --- BINDISP  SEC for first epochs: R8 :: N_PSVSTA
!
      INTEGER*4        STS_BDSFSC(MAX__POSVAR)
      INTEGER*8 :: ADR_BDSFSC(MAX__POSVAR)
      INTEGER*8        LEN_BDSFSC(MAX__POSVAR)
!
! --- BINDISP  MJD for last epochs: I4 :: N_PSVSTA
!
      INTEGER*4        STS_BDSLMJ(MAX__POSVAR)
      INTEGER*8 :: ADR_BDSLMJ(MAX__POSVAR)
      INTEGER*8        LEN_BDSLMJ(MAX__POSVAR)
!
! --- BINDISP  SEC for last epochs: R8 :: N_PSVSTA
!
      INTEGER*4        STS_BDSLSC(MAX__POSVAR)
      INTEGER*8 :: ADR_BDSLSC(MAX__POSVAR)
      INTEGER*8        LEN_BDSLSC(MAX__POSVAR)
!
! --- BINDISP: number of samples: I4 :: N_PSVSTA
!
      INTEGER*4        STS_BDSNSA(MAX__POSVAR)
      INTEGER*8 :: ADR_BDSNSA(MAX__POSVAR)
      INTEGER*8        LEN_BDSNSA(MAX__POSVAR)
!
! --- Date of last modification of th file with position variations in native
! --- UNIX format
!
      INTEGER*4  TIM_PSVFIL(MAX__POSVAR)
      CHARACTER  BDS_ENDIAN(MAX__POSVAR)*1
      CHARACTER  BDS_FLOAT(MAX__POSVAR)*1
!
      LOGICAL*4   ESTIMATE_NUTATION_FIRST, &
     &            ESTIMATE_UT1_RATE_FIRST, &
     &            ESTIMATE_STATION_FIRST, &
     &            ESTIMATE_EOP_FIRST, &
     &            MAPPED_EOP_OUTPUT, &
     &            SEG_OUTPUT, &
     &            PAMB_MSC, &
     &            MEMFAULT_REP, &
     &            COR_FLAG, COR_GG_FLAG, COR_GL_FLAG, COR_LL_FLAG, &
     &            COR_CL_FLAG
      LOGICAL*1   CNPLT_SUPR_PRE98, CNPLT_SUPR_SIMPLE, &
     &            CNPLT_SHOW_UNRC, CNPLT_SHOW_CBAD, CNPLT_PHASE_S
      INTEGER*2   CRES_STYLE
      LOGICAL*2   G_WARNING
      INTEGER*2   SUPMET_BAT, PAMB_PSTA(2)
      INTEGER*4   MDLPL_IPS_PAG,   MDLPL_IPC_PAG
      INTEGER*4   COROUT_FORM
      LOGICAL*4   MDLPL_FL_FRAME, MDLPL_FL_EOPMOD, MDLPL_FL_CLF
      LOGICAL*4   APRIORI_ZENDEL, DTEC_SBA_USE
      CHARACTER*128  WEB_DIR
      CHARACTER*128  COR_GG_INCFIL, COR_GG_EXCFIL
      CHARACTER*128  COR_GL_INCFIL, COR_GL_EXCFIL
      CHARACTER*128  COR_LL_INCFIL, COR_LL_EXCFIL
      CHARACTER*128  COR_CL_INCFIL, COR_CL_EXCFIL
      CHARACTER*128  COR_CL_INCSES, COR_CL_EXCSES
      CHARACTER*128  CONTROL_FILE
      CHARACTER*128  SUP_CONFIG
      CHARACTER*128  VTD_CONF_GLB, VTD_CONF_SES, EDIT_FIL, AOC_FIL, ADDW_FIL, DTEC_FIL, ATD_FIL, ATD_MEA_FIL, ASM_STA_FIL
      CHARACTER      ASM_NAM*32
      INTEGER*4      DTEC_USE, ATD_USE, ATD_MEA_USE, ATD_STS, ADDW_USE
      REAL*8         TEC_SCAL, TEC_BIAS, ADDW_SCL, DTEC_ERR_SCL
      INTEGER*8 :: ATD_ADR
!
      LOGICAL*2      FL_SINEX_MAKE, FL_SINEX_GLO,  FL_SINEX_LOC, &
     &               FL_SINEX_SEG,  FL_SINEX_EST,  FL_SINEX_COV, &
     &               FL_SINEX_CNS,  FL_SINEX_DCM, &
     &               FL_NRD_TABLE,  FL_CHI_TABLE
      CHARACTER      SINEX_OUTFILE*128, SINEX_INCLUDE*128, &
     &               SINEX_EXCLUDE*128, SINEX_ACKFIL*128,  SINEX_COMFIL*128, &
     &               SINEX_REALNAME*128, SINEX_VERS*4
      CHARACTER      POSVAR_FIL(MAX__POSVAR)*128
      CHARACTER      STASUB2_CHR*128, VELSUB2_CHR*128, PLTSUB2_CHR*128, &
                     ECCSUB_CHR*128,  MGRSUB_CHR*128,  METSUB_CHR*128
      INTEGER*4      N_POSVAR, &
     &               POSVAR_MOD(MAX__POSVAR), POSVAR_INT(MAX__POSVAR), &
     &               POSVAR_USE(MAX__POSVAR)
!
      REAL*8     PLATE_SCL
      REAL*8     JDATE_ALL_BEG, &  ! Date of beginning first used observation
     &           JDATE_ALL_MID, &  ! Weighted mean date
     &           JDATE_ALL_END, &  ! Date of the end of the last used observation
     &           JDATE_STA_BEG(MAX_ARC_STA), &  ! Date of first used observation
     &           JDATE_STA_MID(MAX_ARC_STA), &  ! Weighted mean date
     &           JDATE_STA_END(MAX_ARC_STA)  ! Date of last used observation
      REAL*8     NNT_POS_RTP(3), NNR_POS_RTP(3), &
     &           NNT_VEL_RTP(3), NNR_VEL_RTP(3), &
     &           NNR_SRC_RTP(3), NNR_PRP_RTP(3)
      INTEGER*4  STATUS_JDATE ! Status of Jdate-s 0: defined, not-defined
      INTEGER*4  STAT_NUMOBS, STAT_NUMUNK
      INTEGER*4  STAT_HEO, L_HEO
      INTEGER*8 :: ADR_HEO
      REAL*8           HEO_EPOCH_SEC
      INTEGER*8 :: VTD_ADR
      INTEGER*4        VTD_STATUS
      INTEGER*4        MJD_EHEO_REF
      INTEGER*4        L_EHEO, L_EHEC, L_AEM
      INTEGER*8 :: ADR_EHEO, ADR_EHEC, ADR_EHES, ADR_AEM
      CHARACTER  FINAM_HEO*128, NAME_HEO*80, FRINGE_ROOT_DIR*128, BATCH_CNF_FINAM*128
      CHARACTER  SUPPRESS_FILE*128
      LOGICAL*2  GRAD_AUG2003_BUG, FL_NOFLYBY, FL_VTD_GLB, FL_VTD_SES
      REAL*8     STAT_SQUOC, STAT_SQURES, STAT_VARFAC, STAT_WRMS, TAI_EHEO_REF
      INTEGER*2  SOU_ADM_FLAG, IONOV_USE
      INTEGER*4  TPD_FLAG
      INTEGER*4  RWT_EL_USE, RWT_SRC_USE
      REAL*8     SOU_ADM_CNS
      REAL*8     RWT_EL_GLB
      REAL*8     SNR_MIN_X, SNR_MIN_S, GLO_SNR_MIN_X, GLO_SNR_MIN_S, IONO_ERR_FCT, &
     &           SESS_REWEI_SCALE, SESS_REWEI_QUADR
      CHARACTER  RWT_STA_EL_FIL*128, RWT_SRC_FIL*128
      INTEGER*4  EDC_USE, EDC_PAR, N_TPD_INIT
      CHARACTER  EDC_DIR*128, TPD_DIR*128, TPD_INIT_LIST(M__TPD_INIT)*2
      CHARACTER  TRP_DIR*128, IONOV_DIR*128, VCAT_CONF_FILE*128, VCAT_REPO*4
      INTEGER*4  NUT_USE_CODE
      INTEGER*4  TRP_USE, N_FIL_TRP, STS_TRP_FIL, STS_TRP_SES, STS_TRP, FRTYPFIT
      INTEGER*8 :: ADR_TRP_FIL_BUF, ADR_TRP_SES_BUF, ADR_TRP
      INTEGER*4  SEED_INIT_I4
!
! --- Default uncertainties of external EOP time series
!
      REAL*8       DEF__WOBBLE_SIGMA_MAS, DEF__UT1_SIGMA_MSEC
      PARAMETER  ( DEF__WOBBLE_SIGMA_MAS = 0.1D0   )
      PARAMETER  ( DEF__UT1_SIGMA_MSEC   = 0.005D0 )
      CHARACTER  GIM_INFO_DIR*128, GIM_DTEC_DIR*128, GIM_ADDW_DIR*128, GIM_DEL_DIR*128, &
     &           GIM_NOI_DIR*128, BCL_FIL*128, BRK_FIL*128, EXT_ERR_FIL*128
      REAL*8     GIM_TIM_STEP, GIM_SCALE 
      INTEGER*4  GIM_MODE, GIM_DEG, GIM_SEED, GIM_VERB
      LOGICAL*4  GIM_COLLECT_INFO, GIM_EST, GIM_RGR, GIM_WRI
!
! SITDIF: the site position difference between the substituted and the
!         original site positions
!
! STRDIF: the source position difference
!
! DPRIN:  the nutation principal term difference
! DDECA:  the nutation 9.3 years term difference
! DANNU:  the nutation annual    term difference
! DSEMA:  the nutation semiannual term difference
! D122D:  the nutation 122.7 days term difference
! DSEMM:  the nutation 13.7 days  term difference
!
! DNUT:   the Julian date of nutation "daily offset" series
! DDPSI:  the longitude offset of nutation "daily offset" series
! DDEPS:  the obliquity offset of nutation "daily offset" series
!
! UT1PTV: UT1 data (TAI-UT1) from E.O. mod file corresponding to
!         to UT1INV info
! WOBXXV: X-component of polar motion in milliarseconds from the E.O.
!         mod file corresponding to WOBINV info
! WOBYYV: Y-component of polar motion in milliarseconds from the E.O.
!         mod file corresponding to WOBINV info
! UT1INV: UT1 information from the E.O. mod file as follows: first J.D.,
!         interval between epochs, and number of epochs
! WOBINV: Polar motion info from the E.O. mod file as follows: first J.D.,
!         interval between epochs, and number of epochs
!
! PMDIF:  the site position change due to plate motion
!
! STAXYZ_CNST array of sigmas of constraints on X- Y- Z- station positions
! STAUEN_CNST array of sigmas of constraints on U- E- N- station positions
! VELXYZ_CNST array of sigmas of constraints on X- Y- Z- station velocity
! VELUEN_CNST array of sigmas of constraints on U- E- N- station velocity
! STAXYZ_CNSB bit field of imposing contraints on X- Y- Z- components of
!             station positoins
! STAUEN_CNSB bit field of imposing contraints on U- E- N- components of
!             station positoins
! VELXYZ_CNSB bit field of imposing contraints on X- Y- Z- components of
!             station positoins
! VELUEN_CNSB bit field of imposing contraints on U- E- N- components of
!             station velocities
! STAXYZ_CNFL flag of selection of X- Y- Z- constraints on station postions
! STAUEN_CNFL flag of selection of U- E- N- constraints on station postions
! VELXYZ_CNFL flag of selection of X- Y- Z- constraints on station velocities
! VELUEN_CNFL flag of selection of U- E- N- constraints on station velocities
! ISTAP:  Site position substitution status flag. Zero means no substitution.
! ISOUC:  Source position substitution status flag. Zero means no
!         substitution.
! INUTS:  Nutation parameter substitution status flag. Zero means no
!         substitution.
! INUTD:  Nutation daily series substitution status flag. Zero means no
!         substitution.
! IEROT:  Earth rotation series substitution status flag. Zero means no
!         substitution.
! ITECT:  Tectonic plate motion. Zero means plate motion model not applied.
! NESM:   Number of site motion discontinuities
! KECC:   Flag. If .TRUE. then FLYBY_MAP should apply correction for flyby
!         eccentricity
! KMGR:   Flag. If .TRUE. then FLYBY_MAP should apply correction for mean
!         troposphere gradient
! KMET:   Flag. If .TRUE. then FLYBY_MAP should apply correction for metric
!         tensor
! ESMSITES: Site names of known motion discontinuities
! ESMDATES: Dates of known motion discontinuities (yymmddhh)
! FLYBY_INTERP: Flag to indicate flyby UT1PM interpolation (1= linear,
!               3= cubic, and 0= default).
! FAST_MODE: Flag to indicate appliance of fast algorithms (0 -- none,
!            1 -- fast dot vector product (sparseness!), 2 -- B3D-algorithm,
!            3 -- arc squeezing trick for GLOBL, 4 -- B1B3D
! FAST_DBG:  Debug mode for fast alghorithms (0 -- no debug -- working mode,
!            1 -- indicate passing through key routines, 2 -- usefull printout
!            of intermediate stuff, 11 -- time printout, 12 - timing profile
! FAST_COV:  switch: calculate or not some covariance matrix. 1 -- calculate
!            matrix of global-global parameters; 2 -- the same as 1 plus
!            local-global and local-local matrices; 3 -- the same as 2 plus
!            segmented-segmented, segmented-local and segmented-global matrices
!            Default: 2
! FAST_MODE_GLO: global variant FAST_MODE
! FAST_DBG_GLO:  global variant FAST_DBG
! FAST_COV_GLO:  global variant FAST_COV
!                In the case if the mode for one session of global solution
!                changed, the trial values of FSAT_xxx variables for the next
!                session will be setup FAST_xxx_GLO. Thus FAST_xxx_GLO saves
!                "global" values FAST_xxx variables.
!
! REWAY_ITCOU:   REWAY iteration counter
! REWAY_FLODEL:  Floor for reweight constant (in psec) for delay.
! REWAY_FLORATE: Floor for reweight constant (in fs/sec) for delay rate.
! REWAY_CEIDEL:  Ceiling for reweight constant (in psec) for delay.
! REWAY_CEIRATE: Ceiling for reweight constant (in fs/sec) for delay rate.
! REWAY_CHITOL:  Tolerance Chi_sq/NDF, This parameter is used to decide when it
!                is time to stop reweighing
! REWAY_MAXIT:   The maximum number of iterations for updating weights
! REWAY_VERBOSE: Verbosity mode for program REWAY (applicable only
!                to interavtive mode).
! REWAY_TYPE:    One of possible types for REWAY:
!                BA -- baseline-dependatnt reway
!                SI -- site-dependant reway
!                GL -- global reway
!                NO -- the type which has been written in database. If no type
!                      has been written then baseline type.
! REWAY_NEWUPD:   If true then new (for 21-JAN-98), experimental mode used
!                 for updating weights
! REWAY_FALLBACK: Mode when automatic change of the reweighting type is allowed:
!                 baseline --> station --> global
!
! GAMB_F_X_BAND: Logical variable: whether to make group delay ambiguity
!                resolution for X-band data if programm GAMB is called
! GAMB_F_S_BAND: Logical variable: whether to make group delay ambiguity
!                resolution for S-band data if programm GAMB is called
! GAMB_F_ION:    Logical variable: whether to calculcate ionosphere correction
!                just after group delay ambiguity resolution
! GAMB_F_PREUSE: Logical variable: whether to use only good observations
!                (.TRUE.) when make group delay ambiguity resolution or
!                all observations (.FALSE.)
! GAMB_F_SAVE:   Logical variable: whether to save informations about outliers
!                detected during group delay ambiguity resolution (.TRUE.) or
!                not
! GAMB_CUTOFF:   cutoff limit for detecting outliers during group delay
!                ambiguity resolution.
! GAMB_MINOBS:   Minimal acceptable number of observations at one baseline for
!                group delay ambigutiy resolution.
! GAMB_SPACING_CONST: Group delay ambiguity spacing constant. If zero then
!                the constant from LCODE GAMBC (and saved in socom as FAMB) will
!                be actiually used
! GAMB_IT:       Verbosity mode parameter for GAMB. 0 means silient mode, 5 --
!                maximal verbosity (2 -- recommenmded verbosity).
! GAMB_BATCH:    If .TRUE. and SOLVE called in batch mode than it will make
!                only resolving group delay ambiguities, but not to parameter
!                estimation. GAMB_BATCH is ignored in interactive mode.
! IONO_VERBOSE:  Verbosity mode parameter for IONO. .TRUE. -- to make
!                debugging printout.  .FALSE. -- suppress debugging printout.
! IONO_SEEK:     S-band search window for ionosphere calibraion procedure
! SETFL_MDEG:    MAX degree of global clock polinomial as default value for
!                interactive SETFL setup.
! DEF_ATMSPAN:   Default duration of one segment span for atmopshere in sec.
!                Used by setfl.
! DEF_CLOSPAN:   Default duration of one segment span for atmopshere in sec.
!                Used by setfl.
! QUALCODE_GOOD_LIM: Mininal value of quality code when the observation consider
!                    as a good one. letter-like quality codes ("A", "B", "C",
!                    "D", "E" are considered as negative and therefore bad )
! SOLVE_EDITOR     16-symbols line of the editor which will be called to edit
!                  something.
! OLD_SOLVE_PS_VIEWER  16-symbols line of the postscript viewer which may be called
!                  from SOLVE.
! SOLVE_EMULATION  Integer flag of the mode compatability withe the previous
!                   version of the SOLVE. 0 means proper mode. 9612 means
!                  "pre-fast" mode.
! ELIM_MOD:      Mode of work ELIM/MILE. .TRUE.  means outliers elimination,
!                                        .FALSE. means outliers restoration
! ELIM_CNF:      Confirmation switcher for ELIM. .TRUE. means that all
!                operations in ELIM/MILE will require user confirmation
! ELIM_THR:      Threshold of outliers detection (in seconds) in ELIM
! ELIM_CUT:      Cutoff for outliers detection  (in sigmas)  in ELIM
! ELIM_MSR:      Maximal sigma for restoration by MILE
! ELIM_VRB:      Verbosity mode for ELIM (0 -- silent mode, 2 normal mode,
!                4 the most verbose mode)
! ELIM_TYP:      Type of cutoff for outliers detection in ELIM: 'BA' --
!                baseline-dependent, 'GL' -- global type
! ELIM_UPD:      How frequently update of the residuals will be made after
!                operation elimination/restoration. 1 means each time.
!                In general residuals will be recalculated after
!                elimination/restoration ELIM_UPD points.
! COND_WARNING:  Specify the maximum accpetable condition number. If condition
!                number of the matrix to be inverted appear higher the
!                specified limit then warning or error message will be
!                generated.
! SEG_OUTPUT:    Logical variable. When .FALSE. informaition about the values of
!                segmented parameters and thier uncertainties will not be
!                included unto listing. Whwn .TRUE. -- values and sigas for all
!                epochs of segmented parameters will be included in listing
! MAPPED_EOP_OUTPUT: If true, EOP values are mapped to fixed times at the
!                    beginning and the end of the the session and listed in the
!                    SPOOLFILE. Used to facilitate EOP comparisons for sessions
!                    on adjacent days.
! MAPPED_EOP_TIME:   Time in seconds for which mapped eop will be calculated
! ESTIMATE_NUTATION_FIRST Whether or not set mode "estimate nutation" in
!                         interactive SOLVE when database/superfile is just read
! ESTIMATE_UT1_RATE_FIRST Whether or not set mode "estimate ut1 rate" in
!                         interactive SOLVE when database/superfile is just read
! ESTIMATE_STATION_FIRST  Whether or not set mode "estimate staion positions" in
!                         interactive SOLVE when database/superfile is just read
! ESTIMATE_EOP_FIRST      Whether or not set mode "estimate pole coodrinates
!                         and Ut1" in interactive SOLVE when database/superfile
!                         is just read
! RCOND_CGM               Condition number of the CGM.
! CRES_PRE98              Flag: if TRUE the mode of compatibility with PRE98
!                         version will be maintaned (suppression strategy)
! CNPLT_SUPR_PRE98        Flag: if TRUE the mode of compatibility with PRE98
!                         version will be maintaned (suppression strategy)
! CNPLT_SUPR_SIMPLE       Flag: if TRUE then suppression scheme "SIMPLE" will
!                         be used by CNPLT.
! CNPLT_SHOW_UNRC         Flag: if TRUE unrecoverable observations will be
!                         displayed at plots by CNPLT
! CNPLT_SHOW_CBAD         Flag: if TRUE conditionally bad observations will be
!                         displayed at plots by CNPLT
! CNPLT_PHASE_S           Flag: if TRUE then CNPLT will calculate phase o-c
!                               for S-band in the more "phase from group"
!                         displayed at plots by CNPLT
!   LIN_STA_SIGMA         Sigma of cnstr. for linear combination of stat. pos.
!   BAS_CLK_SIGMA         Sigma of cnstr. for baseline-dependent clocks
!   SRC_COO_SIGMA         Sigma of cnstr. for source coordinates
!   SRC_PRP_SIGMA         Sigma of cnstr. for source proper motions
!   NNT_POS_SIGMA         Sigma of cnstr. for no-net translation for stat. pos
!   NNR_POS_SIGMA         Sigma of cnstr. for no-net rotation station position
!   NNT_VEL_SIGMA         Sigma of cnstr. for no-net translation velocities
!   NNR_VEL_SIGMA         Sigma of cnstr. for no-net rotation velocities
!   STA_WEA_SIGMA         Sigma of cnstr. for station positions
!   VEL_WEA_SIGMA         Sigma of cnstr. for velocities
!   VEL_DIR_SIGMA         Sigma of cnstr. for velocity direction
!   VEL_CMP_SIGMA         Sigma of cnstr. for velocity components
!   STA_ORG_SIGMA         Sigma of cnstr. for station positions origin
!   VEL_ORG_SIGMA         Sigma of cnstr. for velocity origin
!   VEL_SET_SIGMA         Sigma of cnstr. for set of velicities
!   STA_TIE_SIGMA         Sigma of cnstr. for station ties
!   VEL_TIE_SIGMA         Sigma of cnstr. for velocity ties
!   RAS_ORG_SIGMA         Sigma of cnstr. for right ascension origin
!   DCL_ORG_SIGMA         Sigma of cnstr. for declination origin
!   NNR_SRC_SIGMA         Sigma of cnstr. for no-net rotation for sources
!   NNR_PRP_SIGMA         Sigma of cnstr. for no-net rotation for proper
!                                         of motion sources
!   NUT_CMP_SIGMA         Sigma of cnstr. for nutation components
!   VEL_VER_SIGMA         Sigma of cnstr. for vertival velocity
! TITLE_ANON              Flag: if TRUE spool file will have "anonymous" title
! TITLE_PRE98             Flag: if TRUE the mode of compatibility with PRE98
!                         version for generating title of SPOOL file will be
!                         maintained
! SUPMET_BAT              Suppression method used as default in batch runs
!
! SNGCHK_ACTION           Code of the action after singularity detection
! SNGCHK_SOUMIN           Minimal number of the observations of the source
!                                 which coordinates are estimated in order
!                                 to trigger singularity detection
! SNGCHK_STAMIN           Minimal number of the observations of the station
!                                 in order to trigger singularity detection
! SNGCHK_BASMIN           Minimal number of the observations made at the
!                                 baseline in order to trigger singularity
!                                 detection
! TRAIN                   Flag of SOLVE configuration. TRUE means to call
!                         as a sequence of small executables.
! GLO_PARLIM              Number of global parameters for which space for CGM
!                         is reserved from the very beginning.
! INC_PARLIM              The portion (in adjusted parameters) of increasing
!                         CGM size in the case when the space for CGM reserved
!                         in the beginning appeared insufficient.
! SORT_SOU                Code of sorting method for sources  in global mode
! SORT_STA                Code of sorting method for stations in global mode
! PAMB_VRB                Verbosity level for PAMB (0-silent mode, 1-- usual)
! PAMB_PARU_FILE          File name for utility PARU
! PAMB_PLOT_TYPE          Type of data to be plotted in PAMB
! PAMB_PLOT_BAND          Code of band: X or S to be plotted in PAMB
! PAMB_PSL_TYPE           Type of suppression criteria to be used by PAMB when
!                              it plots various data
!
! G_WARNING               flag: if TRUE then warnings will be printed in the
!                         screen (and put in spool file in the case of batch
!                         solution)
! MEMFAULT_REP            flag: if TRUE then session is reprocessed anew
!                         if a memory fault occurred during forward run of
!                         global batch solution in NO TRAIN mode. This trick
!                         is done to avoid address space fragmentation.
! MDLPL_IPS_PAG           First page number at the plot of postfit residuals
! MDLPL_IPC_PAG           First page number at the plot of residuals + clocks
! MLDLP_FL_EOPMOD         Flag: TRUE means to show high frequency EOP model in
!                               the plot
! MDLPL_FL_CLF            Flag: TRUE means to show clock function in the plots
!                               of residuals + clocks
! MDLPL_FL_FRAME          Flag. If .TRUE. then the plotting frame used in
!                               MDLPL is "global" -- the same for all stations.
!                               If .FALSE. then the individual frame:
!                         min, max is set for each station.
! WEB_DIR                 keeps directory name where plots for Web should be
!                         put. May contain a path to the directory and prefix
! COROUT_FORM             Flag: if .TRUE. arc-files created in forward run
!                         of global solution will not be purged in backward run
! COR_FLAG                Flag: if .TRUE. correlations between the estimates
!                               of parameters should be computed
! COR_GG_FLAG             Flag: if .TRUE. correlations between global-global
!                               parameters are to be computed
! COR_GL_FLAG             Flag: if .TRUE. correlations between global-local
!                               parameters are to be computed
! COR_LL_FLAG             Flag: if .TRUE. correlations between local-local
!                               parameters are to be computed
! COR_CL_FLAG             Flag: if .TRUE. correlations between closs-local
!                               parameters are to be computed (local parameters
!                               between different experiemtns)
! COROUT_FORM             Output format for making correlations.
! COR_GG_INCFIL           File name of the include_parameters for global-global
!                              correlations
! COR_GG_EXCFIL           File name of the exclude_parameters for global-global
!                              correlations
! COR_GL_INCFIL           File name of the include_parameters for global-local
!                              correlations
! COR_GL_EXCFIL           File name of the exclude_parameters for global-local
!                              correlations
! COR_LL_INCFIL           File name of the include_parameters for local-local
!                              correlations
! COR_LL_EXCFIL           File name of the exclude_parameters for local-local
!                              correlations
! COR_CL_INCFIL           File name of the include_parameters for cross-local
!                              correlations
! COR_CL_EXCFIL           File name of the exclude_parameters for cross-local
!                              correlations
! COR_CL_INCSES           File name of the include_sessions for cross-local
!                              correlations
! COR_CL_EXCSES           File name of the exclude_sessions for cross-local
!                              correlations
! CONTROL_FILE            Name of the original control file if SOLVE is running
!                         in BATCH mode. "Interactive solution" if SOVLE is
!                         running in interactive mode.
! NORATE_FLAG             Flag: if TRUE then part of the code for handling delay
!                         rate will be bypassed and no statistic for delay rate
!                         will be computed.
! EQUMEM_FLAG             Flag: if TRUE then equations of conditions are stored
!                         in memory and may be re-used. It is done for
!                         accelration of some programs.
! UPWEI_FLOOR_GROUP__BATCH  --  Floor for reweight constant (in psec) for
!                               delay in group delay solutions for UPWEI called
!                               in batch mode. Quadratic corrections
!                               to weights will never be less than this value.
! UPWEI_FLOOR_PHASE__BATCH  --  Floor for reweight constant (in psec) for
!                               delay in phase delay solutions for UPWEI called
!                               in batch mode.
! UPWEI_CHITOL__BATCH       --  Tolerance factor for chi/ndg for UPWEI called
!                               in batch mode. If chi/ndg at all baselines
!                               differ from 1 by less that this factor then
!                               iterations will be stopped.
! APRIORI_ZENDEL            --  If .TRUE. than a priori zenith path delay
!                               on the epochs of linear spline will put in
!                               the listing.
! SUP_CONFIG              Name of the superfile catalogue configuration file.
!
! SINEX_OUTFILE  -- Name of Sinex output file (may include templates)
! SINEX_INCLUDE  -- Name of the file with parameters names to be included
!                   in Sinnex listing (may contain wildcards and comments)
! SINEX_EXCLUDE  -- Name of the file with parameters names to be excluded
!                   in Sinnex listing (may contain wildcards and comments)
! SINEX_ACKFIL   -- Name of the acknowledge file to be included in Sinex listing
! SINEX_COMFIL   -- Name of the comments file to be included oin Sinex listing
! SINEX_REALNAME -- Actual name of the Sinex file
!
! JDATE_ALL_BEG  -- Date of beginning first used observation
! JDATE_ALL_MID  -- Weighted mean date of the session
! JDATE_ALL_END  -- Date of the end of the last used observation
! JDATE_STA_BEG  -- Date of first used observation in this session
! JDATE_STA_MID  -- Weighted mean date for this station in this session
! JDATE_STA_END  -- Date of last used observation for this station
! STATUS_JDATE   -- Status of Jdate-s 0: defined, not-defined
!
! 5 parameters below are statistics to be put in Sinex listing
!
! STAT_NUMOBS    -- Statistics: the number of used observations
! STAT_NUMUNK    -- Statistics: the total number of estimated parameters
! STAT_SQUOC     -- Statistics: the square sum of weighted o-c
! STAT_SQURES    -- Statistics: the square sum of weighted residuals
! STAT_VARFAC    -- Statistics: Variance factor -- chi^2/ndg
! STAT_WRMS      -- Statistics: weighted mean root squre of residuals (in sec)
!
! NNT_POS_RTP    -- Vector of right parts of no-net-translation for positions
!                   constraints in mm
! NNR_POS_RTP    -- Vector of right parts of no-net-rotation for positions
!                   constraints in mm
! NNT_VEL_RTP    -- Vector of right parts of no-net-translation for site
!                   velocities constraints in mm/yr
! NNR_VEL_RTP    -- Vector of right parts of no-net-translation for site
!                   velocities constraints in mm/yr
! NNR_SRC_RTP    -- Vector of right parts of no-net-translation for source
!                   position
! NNR_PRP_RTP    -- Vector of right parts of no-net-translation for source
!                   proper motion
!
! --- These are variables for position variatons mapping models
!
! N_POSVAR       -- The number of position variation models applied
! POSVAR_FIL     -- Position variations file (or dircetory) name
! POSVAR_MOD     -- Mode of applying position variation model
! POSVAR_USE     -- Way of using position variation model:
!                   none, required, use
! POSVAR_INT     -- Mode of interpolation of position variation model
!
! POSVAR_USE     -- Way of using position variation model:
!
! N_PSVHAR       -- Number of harmonics
! N_PSVSTA       -- Number of sites
! FL_WARN_POSVAR  - Flag: whether to issue a warning for a missing station in
!                   position variation file
! POSVAR_RD_AREA -- Radius of the applicability area
!
! STS_NAMSIT     -- status  of site name table
! ADR_NAMSIT     -- address of site name table
! LEN_NAMSIT     -- length  of site name table
!
! STS_HARVAL     -- status  of harmonic definition table (phase, freq., accel.)
! ADR_HARVAL     -- address of harmonic definition table
! LEN_HARVAL     -- length  of harmonic definition table
!
! STS_HARDSP     -- status  of harmonic displacement table
! ADR_HARDSP     -- address of harmonic displacement table
! LEN_HARDSP     -- length  of harmonic displacement table
!
! STS_STACOO     -- status  of station coordinates table
! ADR_STACOO     -- address of station coordinates table
! LEN_STACOO     -- length  of station coordinates table
!
! STS_BDSFIL     -- status  of BINDISP file name table
! ADR_BDSFIL     -- address of BINDISP file name table
! LEN_BDSFIL     -- length  of BINDISP file name table
!
! STS_BDSSAM     -- status  of BINDISP sampling interval table
! ADR_BDSSAM     -- address of BINDISP sampling interval table
! LEN_BDSSAM     -- length  of BINDISP sampling interval table
!
! STS_BDSFMJ     -- status  of BINDISP MJD for first epochs
! ADR_BDSFMJ     -- address of BINDISP MJD for first epochs
! LEN_BDSFMJ     -- length  of BINDISP MJD for first epochs
!
! STS_BDSFSC     -- status  of BINDISP  SEC for first epochs
! ADR_BDSFSC     -- address of BINDISP  SEC for first epochs
! LEN_BDSFSC     -- length  of BINDISP  SEC for first epochs
!
! STS_BDSLMJ     -- status  of BINDISP MJD for last epochs
! ADR_BDSLMJ     -- address of BINDISP MJD for last epochs
! LEN_BDSLMJ     -- length  of BINDISP MJD for last epochs
!
! STS_BDSLSC     -- status  of BINDISP  SEC for last epochs
! ADR_BDSLSC     -- address of BINDISP  SEC for last epochs
! LEN_BDSLSC     -- length  of BINDISP  SEC for last epochs
!
! STS_BDSNSA     -- status  of BINDISP: number of samples
! ADR_BDSNSA     -- address of BINDISP: number of samples
! LEN_BDSNSA     -- length  of BINDISP: number of samples
!
! TIM_PSVFIL     -- Date of last modification of the file with position
!                   variations in native UNIX format
! BDS_ENDIAN     -- array of Big-ENDIAN, Little-ENDIAN identifiers
! BDS_FLOAT      -- array of float format specificators
!
! L_HEO          -- Number of Harmonic Earth Orientation variations consituents
! ADR_HEO        -- Address of the data structure with HEO parameters
! STAT_HEO       -- Status of the Harmonic Earth Orientation variations
! FINAM_HEO      -- File name with the Harmonic Earth Orientation variations
! NAME_HEO       -- Name of the Harmonic Earth Orientation
! HEO_EPOCH_SEC  -- Epoch for Harmonic Earth Orientation in seconds alapsed
!                   since J2000.0 epoch
! SRC_LISTING_STYLE -- Style of source listing
! STA_LISTING_STYLE -- Style of station listing
! BAS_LISTING_STYLE -- Style of baseline listing
! SEG_LISTING_STYLE -- Style of time epochs for segmented parameters listing
! VTD_CONF_GLB   -- Name of the global configuration file for VTD
! VTD_CONF_SES   -- Name of the session configuration file for VTD
! FL_VTD_GLB     -- Flag, whether to use global  VTD configuration file
! FL_VTD_SES     -- Flag, whether to use session VTD configuration file
! VTD_ADR        -- Address of the VTD object
! VTD_STATUS     -- Status of the VTD object
! L_HPE          -- The number of elements in the harmonic position esimation
!                   array
! L_SPE          -- The number of elements in the spline position esimation
!                   array
! L_BSP          -- The number of elements in the spline position apriori models
! ADR_HPE        -- The address of the harmonic position estimation array
! ADR_SPE        -- The address of the spline position estimation array
! ADR_BSP        -- The address of the spline position apriori models array
!
! FRINGE_ROOT_DIR -- Name of the root of the fringe directory.
! L_EERM          -- Length of the object for estimation of the Earth Roation
!                    model in bytes or zero if no estimation is to be done
! L_MERM          -- Length of the object for applying the Earth Roation
!                    model in bytes or zero if no applying is to be done
! ADR_EERM        -- Address of the object for estimation of the Earth Roation
!                    model or zero if no estimation is to be done
! ADR_EERM        -- Address of the object for estimation of the Earth Roation
!                    model in bytes or zero if no estimation is to be done
! FINAM_MERM      -- File name with mapping Earth rotation model
! FL_NOFLYBY      -- Flag: if .TRUE. then no mapping of flyby calibrations 
!                    is applied
! L_EHEO          -- The number of elements for estimation of the harmonic 
!                    variations in the Earth orientation parameters
! ADR_EHEO        -- The address of the data structure with descriptions of 
!                    parameters of the harmonic variations in the 
!                    Earth orientation
! MJD_EHEO_REF    -- Reference modified Julian date for estimation of the 
!                    empirical harmonic Earth orientation model
! TAI_EHEO_REF    -- Seconds part of the reference modified Julian date 
!                    for estimation of the empirical harmonic Earth 
!                    orientation model
! BATCH_CNF_FINAM -- Control file for BATCH run
! L_AEM           -- length of the data structure for apriori Earth Rotation Model
!                    or zero
! ADR_AEM         -- Address of the data strcuture for the apriori Earth Rotation Model
! ADR_EHES        -- Address of the data strcuture for the reciproval weights
!                    imposed on harmonic variations in the Earth's rotation
! SOU_ADM_FLAG    -- Flag of source admittance estimation
! SOU_ADM_CNS     -- Reciprical weight for source admittance estimation
!
! EDC_USE         -- Flag of external decimation files usage 
! EDC_PAR         -- Parameter for external decimation files usage 
! EDC_DIR         -- Directory with external decimation files
! TPD_FLAG        -- Flag, whether to use theoretical path delay files
! TPD_DIR         -- Directory for theoretical path delay files
! N_TPD_INIT      -- The number of shared user initials for theoretical path delay files
! TPD_INIT_LIST   -- The list of shared user initials for theoretical path delay files
! RWT_EL_USE      -- Flag of usage of elevation dependent reweighting
! RWT_SRC_USE     -- Flag of usage of source dependent reweighting
! RWT_SRC         -- Source dependent reweighting parameter
! RWT_EL_GLB      -- Global factor before mapping function for elevation 
!                    dependent reweighting
! RWT_STA_EL_FIL  -- Name of the reweighting station elevation file
! RWT_SRC_FIL     -- Name of the reweighting source file
!
! TRP_USE         -- Usage flag of external tropospheric path delay model exchange files
! TRP_DIR         -- Directory with external tropospheric path delay model exchange files
! N_FIL_TRP       -- The total number of external tropospheric path delay model exchange files
! ADR_TRP_FIL_BUF -- Address of the buffer with names of external tropospheric path delay model exchange files
! STS_TRP_FIL     -- Status  of the buffer with names of external tropospheric path delay model exchange files
! ADR_TRP_SES_BUF -- Address of the buffer with sesion names for external tropospheric path delay model exchange files
! STS_TRP_SES     -- Status of the buffer with sesion names for external tropospheric path delay model exchange files
! ADR_TRP         -- Addres of the data structure with contents of external tropospheric path delay model exchange file
! STS_TRP         -- Status of the data structure with contents of external tropospheric path delay model exchange file
! NUT_USE_CODE    -- Code of usage of nutation angles offset estimates
! ATD_USE         -- The flag of using Antenna Termal Deformations
! ATD_MEA_USE     -- The flag whether the the antenna height variations measured at the station will be used.
! ATD_FIL         -- The filename with antenna parameters used for Antenna Termal Deformations computations
! ATD_MEA_FIL     -- The filename with the antenna height variations measured at the station. The file should be in the EPHEDISP format
! AOC_FIL         -- File with apriri path delay corrections
! ATD_ADR         -- Address of the data structure with the antenna information
! ATD_STS         -- Address of the data structure with the antenna status
! SNR_MIN_X       -- Minimum SNR at X-band
! SNR_MIN_S       -- Minimum SNR at S-band
! IONO_ERR_FCT    -- Fraction of the ionosphere variance to be added to the group delay errors
! TEC_NOISE_FILE  -- Name of the file with noise model of the GPS TEC model
! SEED_INIT_I4    -- Seed of the random number generator intialized at the beginning of the batch run
! FL_NO_IONO_DEL  -- Flag that the ionospere contribution should be undone
! FL_EQUAL_EFF_FREQ -- Flag that the ionosphere effective frequencies should be the same for all observations
! IONOV_DIR         -- Directory for the external ionospheric model
! IONOV_USE         -- Action    for the external ionospheric model
! FRTYPFIT          -- Fringe fitting type code
! FL_EQUAL_EFF_FREQ -- flag whether to use equal effective ionospheric freqiences
! EDIT_FIL          -- Edit file
! ADDW_FIL          -- Additive weight file
! ASM_NAM           -- Atmosphere stochastic model name
! ASM_STA_FIL       -- Atmosphere stochastic model station parameter file
! SESS_REWEI_SCALE  -- A scaling factor that will be applied to delay formal uncertainties during 
!                      weight computation after applying reweighting
! SESS_REWEI_QUADR  -- In quadrature variance that will be added to delay formal uncertainties 
!                      during after applying SESS_REWEI_SCALE and reweighting
! DTEC_FIL          -- File with differential Total Electron contents
! DTEC_USE          -- The use of differential Total Electron contents: apply, import, update
! TEC_BIAS          -- Bias in zenith TEC that will be added to the apriori TEC. Units: TECU
! TEC_SCAL          -- Scale factor correction of the ionospheric slant path delay. Units: dimensionless
!                      TEC_IONO_SLANT = TEC_SCAL*(TEC_APR(2)*MAP(2) - TEC_APR(1)*MAP(1)) + TEC_BIAS*(MAP(2) - MAP(1))
!                      where TEC_APR is the zenit TEC 
! ADDW_SCL          -- scaling factor of additive weights taken from ADDW_FIL file
! ADDW_USE          -- parameter that controls how to apply external additive weights
!
!  GIM_INFO_DIR     -- directory name with ionospheric information derived from dual-band observations
!  GIM_DTEC_DIR     -- directory name with differential total electron contents
!  GIM_ADDW_DIR     -- directory name with 
!  GIM_DEL_DIR      -- directory name with 
!  GIM_NOI_DIR      -- directory name with 
!  BCL_FIL          -- filename with information about baseline depdent clock : experiment name, station, the value of baseline-dependent clock offset
!  BRK_FIL          -- filename with baseline clock information: experiment name, station, epoch of the break and the value of the clock break
!  GIM_MODE         -- the mode for computation of ionospheric model, supported modes are 1, 2, and 3
!  GIM_DEG          -- the degree of the B-spline for computation of the ionospheric model
!  GIM_TIM_STEP     -- Time step of the B-spline that models the ionospheric bias
!  GIM_SCALE        -- Scaling factor for apriori TEC from GNSS used for computation of the ionospheric model
!  GIM_COLLECT_INFO -- Flag whether to collect information about TEC from dual-band VLBI data and from GNSS
!  GIM_EST          -- Flag whether to run estimation of a model for TEC bias
!  GIM_RGR          -- Flag whether to compute GNSS TEC errors from the regression model
!  GIM_WRI          -- Flag whether to write adjuisted information about differential TEC to the last version of the database
!  GIM_SEED         -- seed for the random number generator that is used for computation of ionospheric path delay uncertainty
!  GIM_VERB         -- verbosity parameter for computation of differential TEC bias adjustement
!
!  VCAT_CONF_FILE   -- Name of VCAT configuration file
!  VCAT_REPO        -- Name of VCAT repository in upper case
!
! BEGMARK_I4 mark for the first element of the common area
! ENDMARK_I4 mark for the last  element of the common area
!
! GLBC4_FILLER  filler for unused space.
!
! >>>>>> End of file glbc4.i  <<<<<<<<<<
