!@This is the start of file SOCOM (28 block socom)
!
!  changes
!
!   kdb  951207  Integer*4 number of observations.
!
! IF YOU CHANGE THIS FILE YOU MUST ALSO UPDAT Q_SOCOM
!
       Real*8 &
     & PI_28           , FJDCL_28(100)  , TATM_28(300)  , &
     & ELMIN_28        , VLIGHT_28          , TROT_28(12)  , &
     & WRMS_28(3)      , ROTAP_28(12,4)   , UT1INB_28(3)  , &
     & WOBINB_28(3)    , CALCV_28      , &
     & ATMOS_INTERVAL_28     , CLOCK_INTERVAL_28   , INT_ROT_A1_28(2), &
     & TROT_A1_28      , &
     & ut1ptb_28(15), &
     & wobxxb_28(15), &
     & wobyyb_28(15)
!
       Real*8 &
     & FCNPER_28      , SACNST_28(16)    , &
     & SCCNST_28(16)  , ELVCUT_28(16)    , &
     & EOPCONS_28(3)  , eoprcons_28(3)   , SEOCNST_28(2) , pwccnst_28, &
     & NUTCONS_28(2)  , tgrad_28(22), grad_interval_28, gradcons_28(2)
!
       Logical*2 &
     & LOGBCL_28           , BMODE_CL_28       , BMODE_AT_28        , &
     & CLK_BRK_STAT_28     , FLYBY_WARNING_28  , SITE_DEP_CONST_28  , &
     & SIMULATION_TEST_28  , SITE_DEP_EL_CUT_28  , SHORT_UT1_IN_28  , &
     & SOL_AVAIL_28, old_clocks_28   , old_atms_28,  skip_eop_off_28
!
       Character*50 &
     & user_pro_28
       Character*68 &
     & user_buf_28
       Character*4 &
     & scr_fil_origin_28
!
       Integer*2 &
     & NUMSTR_28       , NUMSTA_28        , NUMOBS_28       , &
     & NPOLD_28        , &
     & ICLOCK_28(1,16)       , &
     & IDNWT_28        , IPRES_28         , &
     & IRNCD_28(2)     , ITDGLB_28        , NPARAM_28    , IDATYP_28, &
     & NROT_28         , NSOURC_28        , NSPARM_28(16), &
     & NUMATM_28(16)          , numgrad_28(16)   , &
     & IATSTR_28(16)          , ICLMAX_28       , &
     & NUMCLK_28(16)          , ICLSTR_28(16)    , &
     & IPSTP_28        , LNUT_28(3)       , LPREC_28        , &
     & LTIDE_28(32,3)       , &
     & LREL_28         , LROT_28(3,3)        , &
     & LATM_28(19,3)        , LCLK_28(100)          , &
     & LSTAR_28(48,2)       , LAXOF_28(32)   , &
     & LSITEC_28(32,3)      , &
     & ISRSEL_28(48)        , IUEN_28       , &
     & ICLSTA_28(100)         , NFLEPS_28       , FLEPS_28(14)  , &
     & NFLPSI_28       , &
     & FLPSI_28(14)    , IDPNUT_28(7)     , NDPNUT_28       , &
     & LSITEV_28(32,3)      , &
     & IARCSOC_28      , NSLAST_28        , IDBEND_28(15)  , &
     & IDBSEL_28       , NDB_28           , IDCSEL_28       , &
     & IBLSEL_28(1,16)       , &
     & CONSTRAINT_BITS_28              , INDL_28         , &
     & WVMASK_28(16)       , BM_REF_CL_28    , NROT_A1_28(2), &
     & EOP_STYLE_28(2)              , EOPA1_CHOICE_28(2)          , &
     & IEOPL_28              , numstax_28      , &
     & interpolation_UT1_28            ,interpolation_pm_28, &
     & BGROUND_28      , lprop_28(48,2),  totsta_28, &
     & lgrad_28(2)     , IFREE_SOCOM_28(2)
      character*1 ut1_rs_28, ut1_rs_flyby_28
!
      INTEGER*2 ISOCOM_28(3584),iso1_28(1665),iso2_28(66),iso3_28(960)
      integer*2 iso4_28(735),iso5_28(669),iso6_28(517)
      INTEGER*2 iso1p_28_33(1908),iso2p_28_33(23),iso3p_28_33(9)
      integer*2 iso4p_28_33(541),iso5p_28_33(136),iso6p_28_33(496)
!
       Equivalence (ISOCOM_28, PI_28)
!      Set of pointers for transferring data from "23 block" socom format to
!      "28 block" socom format in cgm_com.f.
       Equivalence (iso1_28,PI_28)
       Equivalence (iso2_28,iarcsoc_28)
       Equivalence (iso3_28,logbcl_28)
       Equivalence (iso4_28,elmin_28)
       Equivalence (iso5_28,lclk_28)
       Equivalence (iso6_28,int_rot_a1_28)
!      Set of pointers for transferring data from "28 block" socom format to
!      "33 block" socom format in cgm_com.f.
       Equivalence (iso1p_28_33,PI_28)
       Equivalence (iso2p_28_33,eopcons_28)
       Equivalence (iso3p_28_33,idnwt_28)
       Equivalence (iso4p_28_33,ipstp_28)
       Equivalence (iso5p_28_33,nfleps_28)
       Equivalence (iso6p_28_33,int_rot_a1_28)
!
!  common
!
       Common /SOCOM_28/ &
!
!  real*8
!
     & PI_28           , FJDCL_28         , TATM_28         , &
     & ELMIN_28        , VLIGHT_28        , TROT_28         , &
     & WRMS_28         , ROTAP_28         , UT1INB_28       , &
     & WOBINB_28       , ut1_rs_28        , ut1_rs_flyby_28 , &
     & calcv_28        , ATMOS_INTERVAL_28, CLOCK_INTERVAL_28         , &
     & FCNPER_28       , SACNST_28        , SCCNST_28       , ELVCUT_28, &
     & EOPCONS_28      , &
!
!  logical*2
!
     & LOGBCL_28           ,BMODE_CL_28       ,BMODE_AT_28        , &
     & CLK_BRK_STAT_28     ,FLYBY_WARNING_28  ,SITE_DEP_CONST_28  , &
     & SIMULATION_TEST_28  ,SITE_DEP_EL_CUT_28 ,SHORT_UT1_IN_28   , &
!
!  integer*2
!
     & NUMSTR_28       , NUMSTA_28        , NUMOBS_28       , &
     & NPOLD_28        , &
     & ICLOCK_28       , &
     & IDNWT_28        , IPRES_28         , &
     & IRNCD_28        , ITDGLB_28        , NPARAM_28       , IDATYP_28, &
     & NROT_28         , NSOURC_28        , NSPARM_28       , NUMATM_28, &
     & IATSTR_28       , ICLMAX_28        , NUMCLK_28       , ICLSTR_28, &
     & IPSTP_28        , LNUT_28          , LPREC_28        , LTIDE_28 , &
     & LREL_28         , LROT_28          , LATM_28         , LCLK_28  , &
     & LSTAR_28        , LAXOF_28         , LSITEC_28       , &
     & ISRSEL_28       , IUEN_28          , &
     & ICLSTA_28       , NFLEPS_28        , FLEPS_28        , NFLPSI_28, &
     & FLPSI_28        , IDPNUT_28        , NDPNUT_28       , LSITEV_28, &
     & IARCSOC_28      , NSLAST_28        , IDBEND_28     , &
     & IDBSEL_28       , NDB_28           , IDCSEL_28       , IBLSEL_28, &
     & CONSTRAINT_BITS_28              , INDL_28         , WVMASK_28   , &
     & BM_REF_CL_28    , &
!
!    Additions
!
     & EOP_STYLE_28    , EOPA1_CHOICE_28 , NROT_A1_28, INT_ROT_A1_28 , &
     & TROT_A1_28      , SEOCNST_28      , ieopl_28  , numstax_28   , &
     & SOL_AVAIL_28    , interpolation_ut1_28,interpolation_pm_28, &
     & BGROUND_28      , old_clocks_28   , old_atms_28 ,eoprcons_28, &
     & ut1ptb_28       , wobxxb_28       , wobyyb_28   ,lprop_28, &
     & pwccnst_28      , totsta_28       , skip_eop_off_28, &
     & user_pro_28     , user_buf_28     , nutcons_28,scr_fil_origin_28, &
     & grad_interval_28, tgrad_28        , gradcons_28 ,numgrad_28  , &
     & lgrad_28        , &
     & ifree_socom_28
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
!
! view of common: (as of 1/30/96)
!                  # words     ending word in common
! PI_28                  4           4.0
! FJDCL_28             400         404.0
! TATM_28             1200        1604.0
! ELMIN_28               4        1608.0
! VLIGHT_28              4        1612.0
! TROT_28               48        1660.0
! WRMS_28               12        1672.0
! ROTAP_28             192        1864.0
! UT1INB_28             12        1876.0
! WOBINB_28             12        1888.0
! ut1_rs_28             .5        1888.5
! ut1_rs_flyby_28       .5        1889.0
! **filler_to_skip**     3        1892.0
! calcv_28               4        1896.0
! ATMOS_INTERVAL_28      4        1900.0
! CLOCK_INTERVAL_28      4        1904.0
! FCNPER_28              4        1908.0
! SACNST_28             64        1972.0
! SCCNST_28             64        2036.0
! ELVCUT_28             64        2100.0
! EOPCONS_28            12        2112.0
! LOGBCL_28              1        2113.0
! BMODE_CL_28            1        2114.0
! BMODE_AT_28            1        2115.0
! CLK_BRK_STAT_28        1        2116.0
! FLYBY_WARNING_28       1        2117.0
! SITE_DEP_CONST_28      1        2118.0
! SIMULATION_TEST_28     1        2119.0
! SITE_DEP_EL_CUT_28     1        2120.0
! SHORT_UT1_IN_28        1        2121.0
! NUMSTR_28              1        2122.0
! NUMSTA_28              1        2123.0
! NUMOBS_28              1        2124.0
! NPOLD_28               1        2125.0
! ICLOCK_28             16        2141.0
! IDNWT_28               1        2142.0
! IPRES_28               1        2143.0
! IRNCD_28               2        2145.0
! ITDGLB_28              1        2146.0
! NPARAM_28              1        2147.0
! IDATYP_28              1        2148.0
! NROT_28                1        2149.0
! NSOURC_28              1        2150.0
! NSPARM_28             16        2166.0
! NUMATM_28             16        2182.0
! IATSTR_28             16        2198.0
! ICLMAX_28              1        2199.0
! NUMCLK_28             16        2215.0
! ICLSTR_28             16        2231.0
! IPSTP_28               1        2232.0
! LNUT_28                3        2235.0
! LPREC_28               1        2236.0
! LTIDE_28              96        2332.0
! LREL_28                1        2333.0
! LROT_28                9        2342.0
! LATM_28               57        2399.0
! LCLK_28              100        2499.0
! LSTAR_28              96        2595.0
! LAXOF_28              32        2627.0
! LSITEC_28             96        2723.0
! ISRSEL_28             48        2771.0
! IUEN_28                1        2772.0
! ICLSTA_28            100        2872.0
! NFLEPS_28              1        2873.0
! FLEPS_28              14        2887.0
! NFLPSI_28              1        2888.0
! FLPSI_28              14        2902.0
! IDPNUT_28              7        2909.0
! NDPNUT_28              1        2910.0
! LSITEV_28             96        3006.0
! IARCSOC_28             1        3007.0
! NSLAST_28              1        3008.0
! IDBEND_28             15        3023.0
! IDBSEL_28              1        3024.0
! NDB_28                 1        3025.0
! IDCSEL_28              1        3026.0
! IBLSEL_28             16        3042.0
! CONSTRAINT_BITS_28     1        3043.0
! INDL_28                1        3044.0
! WVMASK_28             16        3060.0
! BM_REF_CL_28           1        3061.0
! EOP_STYLE_28           2        3063.0
! EOPA1_CHOICE_28        2        3065.0
! NROT_A1_28             2        3067.0
! INT_ROT_A1_28          8        3075.0
! **filler_to_skip**     1        3076.0
! TROT_A1_28             4        3080.0
! SEOCNST_28             8        3088.0
! ieopl_28               1        3089.0
! numstax_28             1        3090.0
! SOL_AVAIL_28           1        3091.0
! interpolation_ut1_28   1        3092.0
! interpolation_pm_28    1        3093.0
! BGROUND_28             1        3094.0
! old_clocks_28          1        3095.0
! old_atms_28            1        3096.0
! **NO_SKIP_NEEDED**     0        3096.0
! eoprcons_28           12        3108.0
! ut1ptb_28             60        3168.0
! wobxxb_28             60        3228.0
! wobyyb_28             60        3288.0
! lprop_28              96        3384.0
! **NO_SKIP_NEEDED**     0        3384.0
! pwccnst_28             4        3388.0
! totsta_28              1        3389.0
! skip_eop_off_28        1        3390.0
! user_pro_28           25        3415.0
! user_buf_28           34        3449.0
! **filler_to_skip**     3        3452.0
! nutcons_28             8        3460.0
! scr_fil_origin_28      2        3462.0
! **filler_to_skip**     2        3464.0
! grad_interval_28       4        3468.0
! tgrad_28              88        3556.0
! gradcons_28            8        3564.0
! numgrad_28            16        3580.0
! lgrad_28               2        3582.0
! ifree_socom_28         2        3584.0
