!@This is the start of file SOCOM_23
!
!
       Real*8 &
     & PI_23           , FJDCL_23(100)  , TATM_23(160)  , &
     & ELMIN_23        , VLIGHT_23          , TROT_23(12)  , &
     & WRMS_23(3)      , ROTAP_23(12,4)   , UT1INB_23(3)  , &
     & WOBINB_23(3)    , CALCV_23      , &
     & ATMOS_INTERVAL_23 , CLOCK_INTERVAL_23 , INT_ROT_A1_23(2), &
     & TROT_A1_23      , &
     & ut1ptb_23(15), &
     & wobxxb_23(15), &
     & wobyyb_23(15)
!
       Real*8 &
     & FCNPER_23       , SACNST_23(16)          , &
     & SCCNST_23(16)          , ELVCUT_23(16)      , &
     & EOPCONS_23(3) , eoprcons_23(3) , SEOCNST_23(2) , pwccnst_23, &
     & NUTCONS_23(2)
!
       Logical*2 &
     & LOGBCL_23           , BMODE_CL_23       , BMODE_AT_23        , &
     & CLK_BRK_STAT_23     , FLYBY_WARNING_23  , SITE_DEP_CONST_23  , &
     & SIMULATION_TEST_23  , SITE_DEP_EL_CUT_23  , SHORT_UT1_IN_23  , &
     & SOL_AVAIL_23   , old_clocks_23  , old_atms_23,  skip_eop_off_23
!
       Character*50 &
     & user_pro_23
       Character*68 &
     & user_buf_23
!
       Integer*2 &
     & NUMSTR_23       , NUMSTA_23        , NUMOBS_23       , &
     & NPOLD_23        , &
     & ICLOCK_23(1,16)       , &
     & IDNWT_23       , IPRES_23         , &
     & IRNCD_23(2)   , ITDGLB_23    , NPARAM_23   , IDATYP_23  , &
     & NROT_23         , NSOURC_23        , NSPARM_23(16)    , &
     & NUMATM_23(16)          , &
     & IATSTR_23(16)          , ICLMAX_23       , &
     & NUMCLK_23(16)          , ICLSTR_23(16)       , &
     & IPSTP_23        , LNUT_23(3)       , LPREC_23     , &
     & LTIDE_23(32,3)       , &
     & LREL_23         , LROT_23(3,3)        , &
     & LATM_23(10,3)        , LCLK_23(100)             , &
     & LSTAR_23(48,2)       , LAXOF_23(32)      , &
     & LSITEC_23(32,3)      , &
     & ISRSEL_23(48)        , IUEN_23       , &
     & ICLSTA_23(100)              , NFLEPS_23   , FLEPS_23(14)  , &
     & NFLPSI_23       , &
     & FLPSI_23(14)    , IDPNUT_23(7)     , NDPNUT_23       , &
     & LSITEV_23(32,3)      , &
     & IARCSOC_23      , NSLAST_23        , IDBEND_23(15)  , &
     & IDBSEL_23       , NDB_23           , IDCSEL_23       , &
     & IBLSEL_23(1,16)       , &
     & CONSTRAINT_BITS_23              , INDL_23         , &
!     . WVMASK_23(16)          , BM_REF_CL_23    , NROT_A1_23(2),
     & WVMASK_23(16)          , BM_REF_CL_23 ,nrot_a1_23(2), &
     & EOP_STYLE_23(2)      , EOPA1_CHOICE_23(2)          , &
     & IEOPL_23              , numstax_23      , &
     & interpolation_UT1_23            ,interpolation_pm_23, &
     & BGROUND_23      , lprop_23(48,2),  totsta_23, &
     & IFREE_SOCOM_23(75)
      character*1 ut1_rs_23, ut1_rs_flyby_23
!
      INTEGER*2 ISOCOM_23(JSOCOM_WORDS),iso1_23(1665)
      INTEGER*2 iso2_23(66),iso3_23(960)
      integer*2 iso4_23(735),iso5_23(669),iso6_23(517)
!
       Equivalence (ISOCOM_23, PI_23)
       Equivalence (iso1_23,PI_23)
       Equivalence (iso2_23,iarcsoc_23)
       Equivalence (iso3_23,logbcl_23)
       Equivalence (iso4_23,elmin_23)
       Equivalence (iso5_23,lclk_23)
       Equivalence (iso6_23,int_rot_a1_23)
!
!  common
!
       Common /SOCOM_23/ &
!
!  real*8
!
     & PI_23           , FJDCL_23         , TATM_23         , &
     & ELMIN_23        , VLIGHT_23        , TROT_23         , &
     & WRMS_23         , ROTAP_23         , UT1INB_23       , &
     & WOBINB_23       , ut1_rs_23        , ut1_rs_flyby_23 , &
     & calcv_23        , ATMOS_INTERVAL_23, CLOCK_INTERVAL_23         , &
     & FCNPER_23    , SACNST_23        , SCCNST_23       , ELVCUT_23  , &
     & EOPCONS_23      , &
!
!  logical*2
!
     & LOGBCL_23           ,BMODE_CL_23       ,BMODE_AT_23        , &
     & CLK_BRK_STAT_23     ,FLYBY_WARNING_23  ,SITE_DEP_CONST_23  , &
     & SIMULATION_TEST_23  ,SITE_DEP_EL_CUT_23 ,SHORT_UT1_IN_23   , &
!
!  integer*2
!
     & NUMSTR_23       , NUMSTA_23        , NUMOBS_23       , &
     & NPOLD_23        , &
     & ICLOCK_23       , &
     & IDNWT_23        , IPRES_23         , &
     & IRNCD_23     , ITDGLB_23        , NPARAM_23       , IDATYP_23  , &
     & NROT_23      , NSOURC_23        , NSPARM_23       , NUMATM_23  , &
     & IATSTR_23    , ICLMAX_23        , NUMCLK_23       , ICLSTR_23  , &
     & IPSTP_23     , LNUT_23          , LPREC_23        , LTIDE_23   , &
     & LREL_23      , LROT_23          , LATM_23         , LCLK_23    , &
     & LSTAR_23        , LAXOF_23         , LSITEC_23       , &
     & ISRSEL_23       , IUEN_23          , &
     & ICLSTA_23       , NFLEPS_23        , FLEPS_23      , NFLPSI_23 , &
     & FLPSI_23        , IDPNUT_23        , NDPNUT_23     , LSITEV_23 , &
     & IARCSOC_23      , NSLAST_23        , IDBEND_23       , &
     & IDBSEL_23       , NDB_23           , IDCSEL_23     , IBLSEL_23 , &
     & CONSTRAINT_BITS_23              , INDL_23        , WVMASK_23   , &
     & BM_REF_CL_23    , &
!
!    Additions
!
!     . EOP_STYLE_23 , EOPA1_CHOICE_23 , NROT_A1_23  , INT_ROT_A1_23   ,
     & eop_style_23,eopa1_choice_23,nrot_a1_23,int_rot_a1_23, &
     & TROT_A1_23   , SEOCNST_23      , ieopl_23    , numstax_23      , &
     & SOL_AVAIL_23 , interpolation_ut1_23       ,interpolation_pm_23, &
     & BGROUND_23      , old_clocks_23   , old_atms_23 ,eoprcons_23, &
     & ut1ptb_23       , wobxxb_23       , wobyyb_23   ,lprop_23, &
     & pwccnst_23      , totsta_23       , skip_eop_off_23, &
     & user_pro_23     , user_buf_23     , nutcons_23  ,ifree_socom_23
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
!      IUEN         UEN PARTIAL CONTROL AND RAIDIAL COMPONENTS
!      NFLPSI       NUMBER OF BITS ON IN FLPSI
!      NFLEPS       NUMBER OF BITS ON IN FLEPS
!      FLPSI        ADDITIONAL PSI FLAG BITS, SEE NUT_PARTS
!      FLEPS        ADDITIONAL EPS FLAG BITS, SEE NUT_PARTS
!      IDPNUT       NUTATION PERIOD FLAGS SEE NUTW
!      NDPNUT       NUMBER OF NUTATION PERIOD FLAGS SEE NUTW
!      FCNPER       FREE CORE NUTATION PERIOD
!      LSITEV(4,3)  SITE VELOCITIES ARRANGED AS IN LSITEC
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
!                   NROT_A1, INT_ROT_A1 and TROT_A1 are used for style 1.
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
!      INT_ROT_A1   Size of epochs
!      ROTAP        Rotation information. First index runs over 20
!                   epochs. Second over UT1, tidal correction to UT1,
!                   x-wobble, and y-wobble values. Filled by BLKCL.
!                   (This information is generated as needed in the
!                    auto style.)
!
!      ELMIN        Minimum elevation for current solution. Radians.
!      IDATYP       Delay data type for current solution. See SETFL code.
!      NPARAM       Total number of parameters estimated in current solution.
!                   Computed by PARCN.
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
!     IARCSOC       NUMBER OF ARC PARAMETERS IN THIS ARC, ONLY USED IN
!                   SAVED ARC FILES
!     CONSTRAINT_BITS One 16-bit word used to store clock and atm constraint
!                   use information.
!     SACNST, SCCNST,
!       SEOCNST     Values of atmosphere, clock and earth orientation
!                     constraints
!     CLK_BRK_STAT  Set to .true. if some site has actual clock break.
!                   Otherwise .false.
!     FLYBY_WARNING If .true., then if some sites or sources are not
!                   in flyby files warning is issured. (Does not operate
!                   in batch mode.)
!     INDL          A variable used by STFLG to keep track of the clock
!                   and atmosphere insert/delete/automatic mode status.
!     SITE_DEP_CONST If true, then different clock and atmosphere
!                   for different sites.
!     SIMULATION_TEST Set to .true. only for simulation data bases.
!     SITE_DEP_EL_CUT If true, the elevation cutoff is iste dependent.
!     WVMASK          Mask for site dependent data selection based on WVR
!                     quality. -1 means accept all data with WVR
!                     calibration; 0 means do not WVR quality at all.
!     EOPCONS         EOP constraint to be RSS'd into FLYBY covariance
!                     diagonals only
!     SOL_AVAIL     True if a solution is available (i.e. if CRES has run)
!     interpolation_ut1  Set to 1 if linear interpolation to be used for
!                     reconstructing ut1 apriori values and 3 for cubic
!                     interpolation.  No other values valid.
!     interpolation_pm   Same as for interpoltion_ut1 except for polar motion.
!     ut1_rs             A 1 character variable which tracks the model used in
!                        calc to remove intermediate time scale variations (so
!                        called fortnightly variations) in UT1 before
!                         interpolation. 'R' for the old UT1R model and the new
!                        'S' for the UT1S. model.
!     ut1_rs_flyby       Same as 'ut1_rs_fly', except it tracks to status for
!                        the flyby eop mod file.
!     :92.12.22:jwr:     ut1_rs added (space taken from free space.)
!     :92.12.27:jwr:     ut1_rs_fly added (space taken from free space.)
