!@This is the start of file SOCOM (46 block socom)
!
! Last modified 2023.07.29_12:32:43
!
! IF YOU CHANGE THIS FILE YOU MUST ALSO UPDATE:
!   1.  Q_SOCOM
!   2.  COPIES OF CGM_COM.F IN THE CUTIL, TRSTA AND RTSRC DIRECTORIES
!       (THE TRSTA AND RTSRC COPIES ARE THE SAME).
!  jwr 92.12.22  ut1_rs added (space taken from free space.)
!  jwr 92.12.27  ut1_rs_fly added (space taken from free space.)
!  kdb 95.12.04  Allow integer*4 number of observations
!  jwr 97.04.03  Added EOP__xxx parameters
!  pet 97.06.05  Added init_interacticve variable
!  pet 97.09.12  Added numsca variable
!  pet 97.10.06  Added clock_ref_bits and socom_last_i2 variables
!  pet 97.10.29  Added IDBEST  variable
!  pet 97.12.01  Added STABIT_G and STABIT_P variables
!  pet 97.12.02  Added CGM_TYPE variable
!  pet 97.12.04  Renamed IBLSEL to IBLSEL_G. Added IBLSEL_P. That means that
!                there will be two baseline selection arrays: one for group
!                delay solution: IBLSEL_G, and one for phase delay solution:
!                IBLSEL_P
!  pet 98.02.11  Added variables OPP_STATUS, PAMB_STATUS
!  pet 98.04.28  Added variable  SUPMET
!  pet 1999.11.09  Added variables NUPSI_DIF, NUTEPS_DIF
!  pet 2000.01.24  Added definition of constants *__CNB
!  pet 2000.06.14  Massive changes. Added: EXP_DESC, EXP_CODE, PI_NAME,
!                                          CORRELATOR_NAME, CORRTYPE, EXP_NUM
!
!                  were increased
!  pet 2000.07.03  Added arrays PHCAL_MODE, PHCAL_MODE_S
!  pet 2000.07.04  Added variable REC_MODE
!  pet 2000.07.05  Added variables NUTPSI_AVE, NUTEPS_AVE
!  pet 2001.01.10  Added variables EOP_TS_CALC, EOP_TS_MODF
!  pet 2001.01.11  Added array     CHISQR
!  pet 2002.10.02  Replaced the Integer*4 variables STABIT_G, STABIT_P with
!                  2-elements INTEGER*2 arrays
!  pet 2003.08.15  Added NPAR_GRAD_STA array
!  pet 2003.08.25  Added NPARAM_AFTER_BASCL, DBNAME_CH, DBNAME_VER variables
!  pet 2005.01.27  Added EXPSERNO
!  pet 2005.11.29  Added CABLE_STS, UTC_M_TAI
!  pet 2005.12.01  Added EDIT_STS
!  pet 2005.12.02  Added ENV_FINAM
!  pet 2007.06.03  Added MK3_DBNM
!  pet 2007.06.05  Added META_SUP
!  pet 2007.07.05  Added variable NUT_XY_AVE(2)
!  pet 2010.02.06  Added variable PIMA_CNT, BAND_NAM
!  pet 2016.12.25  Added variable FUSED_STATUS
!  pet 2017.10.23  Added variable NPAR
!  pet 2018.11.11  Added variable MEAN_EFF_FREQ
!  pet 2021.09.20  Increased dimension of CLOCK_REF_BITS
!  pet 2022.08.21  Added variables IOS_EST, IOS_SIG
!  pet 2023.07.29  Added REPO_NAME variable
!  pet 2023.12.22  Removed REPO_NAME variable
!
       INTEGER*2   EOP__POLY, EOP__RATES_AND_SEGS, EOP__SEGS_ONLY, &
     &             EOP__SINE, SOCOM_IFREE_LEN
!!
       PARAMETER ( EOP__POLY            =    0 )
       PARAMETER ( EOP__RATES_AND_SEGS  =    1 )
       PARAMETER ( EOP__SEGS_ONLY       =    2 )
       PARAMETER ( EOP__SINE            =    3 )
       PARAMETER ( SOCOM_IFREE_LEN      =    2 ) ! 2-bytes words
!!
       REAL*8 &
     &            PI_VAR, &
     &            FJDCL(MAX_CLK), &
     &            TATM(MAX_ATM), &
     &            ELMIN, &
     &            VLIGHT, &
     &            TROT(MAX_ROT), &
     &            WRMS(3), &
     &            ROTAP(MAX_ROT,4), &
     &            UT1INB(3), &
     &            WOBINB(3), &
     &            CALCV, &
     &            ATMOS_INTERVAL, &
     &            CLOCK_INTERVAL, &
     &            ROT_INTERVAL(2), &
     &            TROT_A1, &
     &            UT1PTB(MAX_EROT_VALUES), &
     &            WOBXXB(MAX_EROT_VALUES), &
     &            WOBYYB(MAX_EROT_VALUES), &
     &            FCNPER, &
     &            SACNST(MAX_ARC_STA), &
     &            SCCNST(MAX_ARC_STA), &
     &            ELVCUT(MAX_ARC_STA), &
     &            EOPCONS(3), &
     &            EOPRCONS(3), &
     &            SEOCNST(2), &
     &            PWCCNST, &
     &            NUTCONS(2), &
     &            TGRAD(MAX_GRAD), &
     &            GRAD_INTERVAL, &
     &            GRADCONS(2), &
     &            NUTPSI_DIF, &
     &            NUTEPS_DIF, &
     &            NUTPSI_AVE, &
     &            NUTEPS_AVE, &
     &            CHISQR(3),  &
     &            UTC_M_TAI,  &
     &            NUT_XY_AVE(2), &
     &            MEAN_EFF_FREQ(6), &
     &            IOS_SIG
!
       LOGICAL*2 &
     &            LOGBCL, &
     &            BMODE_CL, &
     &            BMODE_AT, &
     &            CLK_BRK_STAT, &
     &            FLYBY_WARNING, &
     &            SITE_DEP_CONST, &
     &            SIMULATION_TEST, &
     &            SITE_DEP_EL_CUT, &
     &            SHORT_UT1_IN, &
     &            SOL_AVAIL, &
     &            OLD_CLOCKS, &
     &            OLD_ATMS, &
     &            SKIP_EOP_OFF, &
     &            CGM_TYPE, &
     &            META_SUP
!
       CHARACTER  USER_PRO*50
       CHARACTER  USER_BUF*68
       CHARACTER  SCR_FIL_ORIGIN*4
       CHARACTER  EXP_DESC*80
       CHARACTER  EXP_CODE*8
       CHARACTER  PI_NAME*80
       CHARACTER  CORRELATOR_NAME*32
       CHARACTER  CORRTYPE*8
       CHARACTER  REC_MODE*80
!
       CHARACTER  UT1_RS*1
       CHARACTER  UT1_RS_FLYBY*1
       CHARACTER  EOP_TS_CALC*8
       CHARACTER  EOP_TS_MODF*8
       CHARACTER  DBNAME_CH*10
       CHARACTER  MK3_DBNM*10
       CHARACTER  ENV_FINAM*128
       CHARACTER  PIMA_CNT*128
       CHARACTER  BAND_NAM(MAX_BND)*1
       CHARACTER  FILLER16*16  ! not used, can be recycled
!
       INTEGER*2 &
     &           NUMSTR, &
     &           NUMSTA, &
     &           NPOLD, &
     &           ICLOCK(ARC_STA_BIT_WORDS,MAX_ARC_STA), &
     &           ICLOCK_P(ARC_STA_BIT_WORDS,MAX_ARC_STA), &
     &           IDNWT, &
     &           IPRES, &
     &           IRNCD(2), &
     &           ITDGLB, &
     &           NPARAM_I2, &
     &           IDATYP, &
     &           NROT, &
     &           NSOURC, &
     &           NSPARM(MAX_ARC_STA), &
     &           NUMATM(MAX_ARC_STA), &
     &           NUMGRAD(MAX_ARC_STA), &
     &           IATSTR(MAX_ARC_STA), &
     &           ICLMAX, &
     &           NUMCLK(MAX_ARC_STA), &
     &           ICLSTR(MAX_ARC_STA), &
     &           IPSTP, &
     &           LNUT(3), &
     &           LPREC, &
     &           LTIDE(STA_BIT_WORDS,3), &
     &           LREL, &
     &           LROT(ROT_BIT_WORDS,3), &
     &           LATM(ATM_BIT_WORDS,3), &
     &           LCLK(MAX_CLK), &
     &           LSTAR(SRC_BIT_WORDS,2), &
     &           LAXOF(STA_BIT_WORDS), &
     &           LSITEC(STA_BIT_WORDS,3), &
     &           ISRSEL(SRC_BIT_WORDS), &
     &           IUEN, &
     &           ICLSTA(ARC_STA_BIT_WORDS,MAX_CLK), &
     &           NFLEPS, &
     &           FLEPS(14), &
     &           NFLPSI, &
     &           FLPSI(14), &
     &           IDPNUT(7), &
     &           NDPNUT, &
     &           LSITEV(STA_BIT_WORDS,3), &
     &           IARCSOC, &
     &           NSLAST, &
     &           IDBSEL, &
     &           NDB, &
     &           IDCSEL, &
     &           IBLSEL_G(ARC_STA_BIT_WORDS,MAX_ARC_STA), &
     &           IBLSEL_P(ARC_STA_BIT_WORDS,MAX_ARC_STA), &
     &           CONSTRAINT_BITS, &
     &           INDL, &
     &           WVMASK(MAX_ARC_STA), &
     &           BM_REF_CL, &
     &           NROT_A1(2), &
     &           EOP_STYLE(2), &
     &           EOPA1_CHOICE(2), &
     &           IEOPL, &
     &           NUMSTAX
      INTEGER*2 &
     &           INTERPOLATION_UT1, &
     &           INTERPOLATION_PM, &
     &           BGROUND, &
     &           LPROP(SRC_BIT_WORDS,2), &
     &           TOTSTA, &
     &           LGRAD(2), &
     &           INIT_INTERACTIVE, &
     &           CLOCK_REF_BITS(ARC_STA_BIT_WORDS), &
     &           IDBEST, &
     &           OPP_STATUS, &
     &           PAMB_STATUS, &
     &           SUPMET, &
     &           EXP_NUM, &
     &           PHCAL_MODE(MAX_ARC_STA), &
     &           PHCAL_MODE_S(MAX_ARC_STA), &
     &           NPAR_GRAD_STA(MAX_ARC_STA), &
     &           NPARAM_AFTER_BASCL_I2, &
     &           EXPSERNO, &
     &           CABLE_SIGN(MAX_ARC_STA), &
     &           FUSED_STATUS, &
     &           IOS_EST, &
     &           IFREE_SOCOM(SOCOM_IFREE_LEN),  &
     &           SOCOM_LAST_I2
!
      INTEGER*4  NUMOBS, &
     &           IDBEND(MAX_DBS), &
     &           NUMSCA, &
     &           DBNAME_VER, &
     &           EDIT_STS, &
     &           NPARAM, &
     &           NPARAM_AFTER_BASCL
      INTEGER*2  STABIT_G(ARC_STA_BIT_WORDS), &
     &           STABIT_P(ARC_STA_BIT_WORDS)
!
      INTEGER*2  ISOCOM(JSOCOM_WORDS)
      INTEGER*4  SOCOM_FIRST_I2
      INTEGER*2  ISO1G_28_33(1908), ISO2G_28_33(23),  ISO3G_28_33(9)
      INTEGER*2  ISO4G_28_33(541),  ISO5G_28_33(136), ISO6G_28_33(496)
!
      EQUIVALENCE ( ISOCOM, PI_VAR         )
      EQUIVALENCE ( SOCOM_FIRST_I2, PI_VAR )
      EQUIVALENCE ( ISO1G_28_33, PI_VAR    )
      EQUIVALENCE ( ISO2G_28_33, EOPCONS   )
      EQUIVALENCE ( ISO3G_28_33, IDNWT )
      EQUIVALENCE ( ISO4G_28_33, IPSTP )
      EQUIVALENCE ( ISO5G_28_33, NFLEPS )
      EQUIVALENCE ( ISO6G_28_33, ROT_INTERVAL )
!
! --------------------
! |    common        |
! -------------------
!
       Common / SOCOM / &
!
!  real*8
!
     & PI_VAR,  FJDCL,  TATM,    ELMIN,   VLIGHT,   TROT,   WRMS, &
     & ROTAP,   UT1INB, WOBINB,  UT1_RS,            UT1_RS_FLYBY, &
     & CALCV,   ATMOS_INTERVAL,  CLOCK_INTERVAL, &
     & FCNPER,  SACNST,          SCCNST,  ELVCUT,   EOPCONS, &
!
!  logical*2
!
     & LOGBCL,          BMODE_CL,        BMODE_AT,  CLK_BRK_STAT, &
     & FLYBY_WARNING,   SITE_DEP_CONST,  SIMULATION_TEST, &
     & SITE_DEP_EL_CUT, SHORT_UT1_IN, &
!
!  integer*2
!
     & NUMSTR,   NUMSTA, &
!
!  integer*4
!
     & NUMOBS, &
!
!  integer*2
!
     & NPOLD,    ICLOCK,   IDNWT,     IPRES, &
     & IRNCD,    ITDGLB,   NPARAM_I2, IDATYP, &
     & NROT,     NSOURC,   NSPARM,    NUMATM, &
     & IATSTR,   ICLMAX,   NUMCLK,    ICLSTR, &
     & IPSTP,    LNUT,     LPREC,     LTIDE, &
     & LREL,     LROT,     LATM,      LCLK, &
     & LSTAR,    LAXOF,    LSITEC,    ISRSEL, &
     & IUEN,     ICLSTA,   NFLEPS,    FLEPS, &
     & NFLPSI,   FLPSI,    IDPNUT,    NDPNUT, &
     & LSITEV,   IARCSOC,  NSLAST, &
!  integer*4
!
     & IDBEND, &
!
!  integer*2
!
     & IDBSEL,    NDB,     IDCSEL,   IBLSEL_G, &
     & CONSTRAINT_BITS,    INDL,     WVMASK,   BM_REF_CL, &
!
!    Additions
!
     & EOP_STYLE,    EOPA1_CHOICE,       NROT_A1,          ROT_INTERVAL, &
     & TROT_A1,      SEOCNST,            IEOPL,            NUMSTAX, &
     & SOL_AVAIL,    INTERPOLATION_UT1,  INTERPOLATION_PM, BGROUND, &
     & OLD_CLOCKS,   OLD_ATMS,           EOPRCONS,         UT1PTB, &
     & WOBXXB,       WOBYYB,             LPROP,            PWCCNST, &
     & TOTSTA,       SKIP_EOP_OFF,       USER_PRO,         USER_BUF, &
     & NUTCONS,      SCR_FIL_ORIGIN,     GRAD_INTERVAL,    TGRAD, &
     & GRADCONS,     NUMGRAD,            LGRAD,            CLOCK_REF_BITS, &
     & INIT_INTERACTIVE,  IDBEST,        NUMSCA,           STABIT_G, &
     & STABIT_P,     IBLSEL_P,           CGM_TYPE,         OPP_STATUS, &
     & PAMB_STATUS,  SUPMET,             NUTPSI_DIF,       NUTEPS_DIF, &
     & NUTPSI_AVE,   NUTEPS_AVE,         ICLOCK_P,         EXP_NUM, &
     & PI_NAME,      EXP_DESC,           EXP_CODE,         CORRELATOR_NAME, &
     & CORRTYPE,     PHCAL_MODE,         PHCAL_MODE_S,     REC_MODE, &
     & EOP_TS_CALC,  EOP_TS_MODF,        CHISQR,           NPAR_GRAD_STA, &
     & NPARAM_AFTER_BASCL_I2,            DBNAME_VER,       DBNAME_CH,     &
     & EXPSERNO,     UTC_M_TAI,          CABLE_SIGN,       EDIT_STS,      &
     & ENV_FINAM,    MK3_DBNM,           META_SUP,         NUT_XY_AVE,    &
     & PIMA_CNT,     BAND_NAM,           FUSED_STATUS,     NPARAM,        &
     & NPARAM_AFTER_BASCL, MEAN_EFF_FREQ, IOS_SIG,         IOS_EST,       &
     & FILLER16,     IFREE_SOCOM,  SOCOM_LAST_I2
!
! ---- Constants whcih define meaning of bits in constrain_bits
!
       INTEGER*2   EOP_VAL__CNB, ATM_RAT__CNB, CLO_RAT__CNB, &
     &             POL_RAT__CNB, UT1_RAT__CNB, EOR_VAL__CNB, &
     &             NUT_VAL__CNB, GRA_RAT__CNB, &
     &             CLO_DTR__CNB, CLO_DSQ__CNB
        PARAMETER  ( EOP_VAL__CNB =  1 )
        PARAMETER  ( ATM_RAT__CNB =  2 )
        PARAMETER  ( CLO_RAT__CNB =  3 )
        PARAMETER  ( POL_RAT__CNB =  4 )
        PARAMETER  ( UT1_RAT__CNB =  5 )
        PARAMETER  ( EOR_VAL__CNB =  6 )
        PARAMETER  ( NUT_VAL__CNB =  7 )
        PARAMETER  ( GRA_RAT__CNB =  8 )
        PARAMETER  ( CLO_DTR__CNB = 11 )
        PARAMETER  ( CLO_DSQ__CNB = 12 )
!
!  All variables declared so that IMPLICIT NONE will yield no errors.
!
!      Explanations:
!      PI           Pi in 11-digit precision.
!      LSITEC(4,3)  Bit array for station solution status. First
!                   index spans 64 sites, second spans X,Y,Z.
!      LAXOF        Bit array for axis offset solution status. 64 sites.
!      LSTAR(13,2,3)Bit array for source solution status. First index
!                   spans 208 sources, second RA and DEC, and third is
!                   not used. (Third will be used for GPS processing.)
!      LCLK         Bit Array for 100 clock epochs.
!      LATM(7,3)    Bit array for atmosphere solution status for 100
!                   atmosphere epochs. 3 bits per epoch.
!      LREL         Flag word for relativity solution status.
!      IUEN         Uen partial control and raidial components
!      NFLPSI       Number of bits on in flpsi
!      NFLEPS       Number of bits on in fleps
!      FLPSI        Additional psi flag bits, see nut_parts
!      FLEPS        Additional eps flag bits, see nut_parts
!      IDPNUT       Nutation period flags see nutw
!      NDPNUT       Number of nutation period flags see nutw
!      FCNPER       Free core nutation period
!      LSITEV(4,3)  Site velocities arranged as in lsitec
!      IFREEx       Filler.
!      LTIDE(4,3)   Bit array of earth tide solution status.  If set
!                   earth tide global solution (ITDGLB = 0), the first
!                   3 bits hold status of Love l, Love h, and lag angle.
!                   If site dependent solution (ITDGLB = 1), the first index
!                   holds status for 64 stations and second runs over Love l,
!                   Love h, and lag angle.
!      LPREC        Precession constant solution status word.
!      LNUT         Bit array for nutation solution status. First two bits
!                   are used when solving for daily nutation and correspond
!                   to dpsi and deps. The remaining bits used for solving
!                   for terms in the nutation series. Their exact use is
!                   defined by the terms selected in CALC.
!      IPSTP        Number of non-station-dependent parameters estimated.
!      NUMSTA       Total number of stations SOLVE knows about.
!      NUMSTR       Total number of sources SOLVE knows about.
!      ICLSTR       For the Ith station ICLSTR(I) is the number of the
!                   position in LCLK immediately before the position
!                   where the clock flags for the Ith station begin.
!      NUMCLK       The number of clock epochs for each station.
!                   ICLSTR, NUMCLK, and LCLK must be understood together.
!      FJDCL        Contains the Julian dates at midnight of the clock
!                   epochs.
!      ICLMAX       The maximum degree of any clock parameter.
!      IATSTR       IATSTR, and NUMATM do for atmospheres what ICLSTR and
!                   NUMCLK do for clocks.
!      TATM         Complete Julian date, including fraction of day, for
!                   the atmosphere epochs.
!      NSPARM       For station I NSPARM(I) contains the parameter number
!                   for the last parameter before the parameters for station
!                   I. For example NSPARM(1) is alway zero. Computed
!                   in PARCN. NSPARM(51) contains the total number of
!                   site parameters.
!      NSOURC       Number of source parameters estimated.
!      EOP_STYLE    Flag specifying how SOLVE should handle the earth
!                   orientation parameterization.  (Current choices are:
!                   0 - old style - Four orders set independently;
!                     x and y wobble independently parameterized
!                   1 - offset and global rate plus zero through all of the
!                       following: rate breaks, diurnal sine, semi-diurnal sine
!                       X and y wobble will have the same parameterization
!                   LROT,NROT, TROT and ROTAP are used for style 0.
!                   NROT_A1, ROT_INTERVAL and TROT_A1 are used for style 1.
!      EOPA1_CHOICE If style 1 is chosen...,
!                     bit 1 on = rate breaks chosen
!                     bit 2    = diurnal sine
!                     bit 3    = semi-diurnal sine
!                     One set of bits for x and y wobble, one set for UT1
!      LROT         Bit array for earth orientation solution. First index
!                   runs over 25 epochs, second over X-pole, Y -pole, and
!                   UT1.
!      NROT,NROT_A1 Number of rotation epochs currently in use.
!      TROT         Compete Julian Date for up to 20 rotation epochs.
!      TROT_A1      Julian date of starting epoch for auto style
!                    (remaining epochs are generated as needed)
!      ROT_INTERVAL Size of epochs
!      ROTAP        Rotation information. First index runs over 20
!                   epochs. Second over UT1, tidal correction to UT1,
!                   x-wobble, and y-wobble values. Filled by BLKCL.
!                   (This information is generated as needed in the
!                    auto style.)
!
!      ELMIN        Minimum elevation for current solution. Radians.
!      IDATYP       Delay data type for current solution. See SETFL code.
!      NPARAM_I2    Total number of parameters estimated in current
!                   solution as a INTEGER*2 variable. Computed by PARCN.
!                   See NPARAM.
!      ITDGLB       Earth-tide global/site-dependent flag. See LTIDE.
!      IRNCD        Solution run code. Computed from date/time of solution.
!      IPRES        Bit array of printing status. Bit 1 on = do print
!                   residuals; bit 2 on = write solution library tape (use
!                   only by NGS; not related to the STERN solution archiving
!                   system); bit 3 on = print correlation matrix.
!      NUMOBS       Total number of observations in the scratch files.
!      VLIGHT       Speed of light (m/s).
!      WRMS         Weighted rms delay, weighed rms rate, combined weighted
!                   rms delay and rate.
!      IDNWT        If = 1, increase delay weights by 1.E9.
!      ICLOCK       Bit array for baseline-dependent clocks.
!      LOGBCL       Set true if any baseline-dependent clocks are estimated.
!      UT1INB       UT1 information from the data base (i.e., either
!                   Rapid Service or Circular D data) as follows:
!                   first J.D., interval between epochs, and number
!                   of epochs.
!      UT1PTB       UT1 data corresponding to UT1NB epochs, i.e., the
!                   values of TAI minus UT1 in seconds.
!      WOBINB       Polar motion information from the data base (i.e.,
!                   either Rapid Service or Circular D data) as
!                   follows: first J.D., interval between epochs, and
!                   number of epochs.
!      WOBXXB       X component of polar motion corresponding to WOBINB
!                   epochs, given in milliarcseconds.
!      WOBYYB       Y component of polar motion corresponding to WOBINB
!                   epochs, given in milliarcseconds.
!      NPOLD        A holding pen for the number of parameters estimated
!                   the last time PARCN was run.  In general, the number
!                   of parameters in the last solution.
!      CALCV        Calc version.
!      ISRSEL       Source selection array.
!      ICLSTA       used to determine if a clock epoch applies
!                   only to this station (= station number), or
!                   to every station but this station (= - station number)
!                   this array is meaningful only if the twelfeth bit
!                   is set in the LCLK word
!     IARCSOC       Number of arc parameters in this arc, only used in
!                   saved arc files
!     CONSTRAINT_BITS One 16-bit word used to store clock and atm constraint
!                     use information.
!     SACNST, SCCNST,
!       SEOCNST       Values of atmosphere, clock and earth orientation
!                      constraints
!     CLK_BRK_STAT  Set to .true. if some site has actual clock break.
!                   Otherwise .false.
!     FLYBY_WARNING If .true., then if some sites or sources are not
!                   in flyby files warning is issured. (Does not operate
!                   in batch mode.)
!     INDL          A variable used by STFLG to keep track of the clock
!                   and atmosphere insert/delete/automatic mode status.
!     SITE_DEP_CONST If true, then different clock and atmosphere
!                    for different sites.
!     SIMULATION_TEST Set to .true. only for simulation data bases.
!     SITE_DEP_EL_CUT If true, the elevation cutoff is iste dependent.
!     WVMASK          Mask for site dependent data selection based on WVR
!                     quality. -1 means accept all data with WVR
!                     calibration; 0 means do not WVR quality at all.
!     EOPCONS         EOP constraint to be RSS'd into FLYBY covariance
!                     diagonals only
!     SOL_AVAIL     True if a solution is available (i.e. if CRES has run)
!     INTERPOLATION_UT1  Set to 1 if linear interpolation to be used for
!                     reconstructing ut1 apriori values and 3 for cubic
!                     interpolation.  No other values valid.
!     INTERPOLATION_PM   Same as for interpoltion_ut1 except for polar motion.
!     UT1_RS             A 1 character variable which tracks the model used in
!                        calc to remove intermediate time scale variations (so
!                        called fortnightly variations) in UT1 before
!                         interpolation. 'R' for the old UT1R model and the new
!                        's' FOR THE ut1s. MODEL.
!     UT1_RS_FLYBY       Same as 'ut1_rs_fly', except it tracks to status for
!                        the flyby eop mod file.
!     INIT_INTERACTIVE   srset(SDBH) set it 1 to flag that socom has been just
!                        initialized. OPTIN set it to 2
!     CLOCK_REF_BITS     filed (max_arc_sta/16 bits) for poitiung the reference station.
!                        It has sence when more then one clock reference
!                        station is used.
!     NUMSCA             Number of common scans in database.
!     IDBEST             bit array to mark whether the database has been
!                        analyzed
!     STABIT_G           Bit field for deslection station for group delay
!                        solution. If i-th station is in group delay solution
!                        then i-th bit in turned on.
!     STABIT_P           Bit field for deslection station for phase delay
!                        solution. If i-th station is in phase delay solution
!                        then i-th bit in turned on.
!     IBLSEL_G           Bit field for group delay solution
!     IBLSEL_P           Bit field for phase delay solution
!     CGM_TYPE           If SOCOM corresponds to single session solution then
!                        .FALSE. If socom corresponds to CGM (global solution)
!                        then .TRUE.
!     OPP_STATUS         Bit field keeping the status of data for the opposite
!                        band
!     PAMB_STATUS        Bit field keeping the status of phase ambiguity
!                        resolution.
!     SUPMET             Method of suppression of observations
!     NUTPSI_DIF         The average diffrence over the session between the
!                        a priori nutation angle and a reference
!                        (IAU 1980 model) nutation angle psi in rad
!     NUTEPS_DIF         The average diffrence over the session between the
!                        a priori nutation angle epsilon and a reference
!                        (IAU 1980 model) nutation angle epsilon in rad
!     NUTPSI_AVE         The average a priori nutation angle psi over the
!                        session
!     NUTEPS_AVE         The average a priori nutation angle epsilon over the
!                        session
!     EXP_NUM            Serial number of the experiment, specific for each
!                        correlator
!     EXP_CODE           Code of the experiment
!     EXP_DESC           Verbose description of the experiment
!     PI_NAME            Name of the agency and/or contact person and/or
!                        principal investigator
!     CORRELATOR_NAME    Name of the correlator
!     CORRTYPE           Type of the correlator, i.g. MK3, MK4, VLBA, K4, S2
!     PHCAL_MODE         Array phase cal modes for each station: -1
!                        undefined, 0 -- manual phase cal, 1 -- measured phase
!                        cal, 2 -- measured phase-cal + phasecal/offset,
!                        3 -- mixed phase-cal.
!     PHCAL_MODE_S       Array phase cal modes for S-band
!     EOP_TS_CALC        Name of the time scaled used for a priori EOP by Calc
!     EOP_TS_MODF        Name of the time scaled used for a priori modfile EOP
!                        by Solve
!     CHISQR(3)          Ratio of sum of squares of weighted residuals to
!                        their mathematical expectation for group delay,
!                        phase delay rate and combined
!     NPAR_GRAD_STA      array which keep the counter of the parameter just
!                        before the first atmosphere gradient parameter of the
!                        first epoch for the ith station. If the atmosphere
!                        gradient for the ith station is not adjusted then
!                        NPAR_GRAD_STA(i) = 0
!     NPARAM_AFTER_BASCL parameter counter just after counting baseline
!                        dependent clocks
!     DBNAME_CH          name of the first database (out of 15 possible)
!     DBNAME_VER         version of the first database (out of 15 possible)
!     EXPSERNO           Internal correlator experiment number
!     CABLE_SIGN         Array of cable signs
!     UTC_M_TAI          UTC minus TAI at the session nominal start.
!     EDIT_STS           Bit field of editing status for different solution types
!     ENV_FINAM          Name of the database envelop file
!     MK3_DBNM           Mark-3 DBH database name
!     META_SUP           Flag, if true, META suppression method and related
!                        variables were initialized
!     NUT_XY_AVE         The average XY nutation (Ginot-Capitaine formalism) over
!                        the observing session
!     PIMA_CNT           PIMA control file name used for generating the GVH database
!     BAND_NAM           Array of one-character lone band names
!     FUSED_STATUS       Status of applying external ionospheric model, which enables FUSED data type
!     NPARAM             Number of parameters as an INTEGER*4 variable
!     MEAN_EFF_FREQ      Mean effective frequences
!     IOS_EST            Ionosphere path delay scale estimation
!     IOS_SIG            Ionosphere path delay scale constraint
!
! <<< End of socom.i
