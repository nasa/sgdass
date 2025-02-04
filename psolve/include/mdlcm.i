!@This is the start of file &MDLCM
!
! modifications
!
! kdb  8/15/95  Handle two high frequency eop files, one for application to
!               the estimated totals and one for plotting for comparison to
!               the totals.  (Previously one model was used for both functions.)
! pet 11/05/97  Added some constants used in MDLPL_EXT
! jwr 10/02/98  do_web_plots variable were added
! pet 12/24/98  Increased length of laserid from 8 to 16 characters.
!               Added array sassuze.
! pet 1999.07.27.  Added parameters IPSF_TYP, IPSC_TYP, ICLF_TYP
! pet 1999.11.04   Added parameter  ICLB_TYP
! pet 2000.02.03   Replaced  value of HFEOP_CMP_DEF  hf1033cd with jmg96.hf
! pet 2000.07.17   INcreased MAX_PTC to 32767
!
!  NB:  solve.i should be declared before mdlcm.i
!
      INTEGER*2 MAX_PTS
      PARAMETER ( MAX_PTS = 32767 )
!
      integer*2 max_clk_params
      parameter (max_clk_params = 4*max_clk)
      logical*2 lbreak          (max_arc_sta,max_clk_params)
      real*8    site_break_times(max_arc_sta,max_clk_params)
      real*8 clock_deriv(                    max_clk_params)
      real*8 ut1_scale_min, ut1_scale_max
      real*8 xp_scale_min,  xp_scale_max
      real*8 yp_scale_min,  yp_scale_max
      integer*2 breaks_per_site( max_arc_sta), start_time(5), &
     &          stop_time(5)
      integer*2 site_break_pointer(max_arc_sta,max_clk_params)
      character laserid*16
      logical*2 add_back_hfjmg, do_web_plots
      character*8  hfeop_sol_file_name,hfeop_cmp_file_name
      character*60 hfeop_sol_file_namr,hfeop_cmp_file_namr
!
      COMMON /MDLCM/ ICOUNT        ,TAB_LEN            , &
     & eop_tag(5,max_pts)          , &
     & SITE_ID  (Max_pts)          ,TIME_TAG(5,Max_pts), &
     & ATM_VAL  (Max_pts)          ,ATM_SIG (  Max_pts), &
     & CLK_VAL  (Max_pts)          ,CLK_SIG (  Max_pts), &
     & Elevation(Max_pts)          ,real_date(  Max_pts), &
     & eop_date(  Max_pts)         , &
     & press_h  (max_pts)          , tempc_h (  max_pts), &
     & humid_h  (max_pts)          , &
     & saszdry  (max_pts)          , saszwet (  max_pts), &
     & saszuse  (max_pts)          , &
     & IWHICH(M_GPA)             , &
     & CLOCK_CONTROL(MAX_ARC_STA)  , ATMOS_CONTROL(MAX_ARC_STA)     , &
     & OUTPUT_CONTROL,BTC(M_GPA)   , DEFAULT_A_SCALE,DEFAULT_C_SCALE, &
     & ATMOS_MIN                   , ATMOS_MAX                      , &
     & CLOCK_MIN                   , CLOCK_MAX                      , &
     & FIRST_TIME                  , LAST_TIME                      , &
     & SET_SCALE_INT_A             , SET_SCALE_INT_C                , &
     & SAVE_PLOT_FILES             , do_web_plots                   , &
     & FJD_START                   , FJD_STOP                       , &
     & IEO_CT                      , EO_VAL,EO_SIG                  , &
     & IEO_TIME_PT                 , mean_eo_off                    , &
     & model_val                   , DEFAULT_EO_SCALE               , &
     & EO_CONTROL                  , hf_eo_ray                      , &
     & hf_eo_tah                   , &
     & tot_z_control(max_arc_sta)  , dry_z_control(max_arc_sta)     , &
     & press_control(max_arc_sta)  , temp__control(max_arc_sta)     , &
     & humid_control(max_arc_sta)  , batch_control                  , &
     & run_id, user_initials       , lbreak                         , &
     & site_break_times            , clock_deriv                    , &
     & breaks_per_site             , site_break_pointer             , &
     & ut1_scale_min               , ut1_scale_max                  , &
     & xp_scale_min                , xp_scale_max                   , &
     & yp_scale_min,   yp_scale_max, laserid                        , &
     & start_time,stop_time        , add_back_hfjmg                 , &
     & hfeop_sol_file_name         , hfeop_sol_file_namr            , &
     & hfeop_cmp_file_name         , hfeop_cmp_file_namr
!
      COMMON /MDLCM_CH/ ALT_PLOT_LU
!
      CHARACTER*18 ALT_PLOT_LU
      character*2 user_initials
!
      INTEGER*2 &
     &SITE_ID          , TIME_TAG        ,ICOUNT           ,TAB_LEN, &
     & IWHICH          , FIRST_TIME(5)   ,LAST_TIME(5)     ,IEO_CT, &
     &IEO_TIME_PT(10000), eop_tag
!
      REAL*8 &
     &ATM_VAL    ,ATM_SIG      ,CLK_VAL          ,CLK_SIG, &
     &ATMOS_MIN  ,ATMOS_MAX    ,CLOCK_MIN        ,CLOCK_MAX, &
     &FJD_START  ,FJD_STOP     ,EO_VAL(3,max_pts),EO_SIG(3,max_pts), &
     &                       model_val(3,max_pts,2),mean_eo_off(3), &
     &                       hf_eo_ray(3,max_pts), &
     &                       hf_eo_tah(3,max_pts), &
     &Elevation  ,press_h      ,tempc_h          ,humid_h  , &
     &saszdry    ,saszwet      ,real_date        ,eop_date
!
      REAL*8 BTC
!
      LOGICAL*2 CLOCK_CONTROL,  ATMOS_CONTROL,   OUTPUT_CONTROL, &
     &          DEFAULT_A_SCALE,                 DEFAULT_C_SCALE, &
     &          SET_SCALE_INT_A,                 SET_SCALE_INT_C, &
     &          SAVE_PLOT_FILES,                 DEFAULT_EO_SCALE, &
     &          EO_CONTROL(3)  , &
     &          tot_z_control  ,  dry_z_control  , &
     &          press_control  ,  temp__control  , &
     &          humid_control  ,  batch_control
      LOGICAL*4  saszuse
!
      CHARACTER*30 RUN_ID
      INTEGER*4  ICLP_TYP, ICLT_TYP, ICLB_TYP, IATP_TYP, IXPL_TYP, &
     &           IYPL_TYP, IUT1_TYP, IPSF_TYP, IPSC_TYP, ICLF_TYP
      PARAMETER  ( ICLP_TYP =  1 )  !  Type "Piese-wise clock function"
      PARAMETER  ( ICLT_TYP =  2 )  !  Type "Total clock function"
      PARAMETER  ( ICLB_TYP =  3 )  !  Type "Total clock function"
      PARAMETER  ( IATP_TYP =  4 )  !  Type "Piese-wise atmosphere path delay"
      PARAMETER  ( IXPL_TYP =  5 )  !  Type "Piese-wise X pole coordinate"
      PARAMETER  ( IYPL_TYP =  6 )  !  Type "Piese-wise Y pole coordinate"
      PARAMETER  ( IUT1_TYP =  7 )  !  Type "Piese-wise UT1 arguments"
      PARAMETER  ( IPSF_TYP =  8 )  !  Type "Post fit residuals"
      PARAMETER  ( IPSC_TYP =  9 )  !  Type "Post fit residuals + clock function"
      PARAMETER  ( ICLF_TYP = 10 )  !  Type "Clock function"
!
! --- Specifying default compare EOP file (used only in MDLPL_EXT ) and
! --- names of help files
!
      CHARACTER    HFEOP_CMP_DEF*8,       MDLPL_EXT_HLPFIL*13, &
     &             MDLPL_PLUS_HLP_OVR*17, MDLPL_PLUS_HLP_ATM*17, &
     &             MDLPL_PLUS_HLP_CLO*17, MDLPL_PLUS_HLP_EOP*17, &
     &             MDLPL_PLUS_HLP_PSF*17, MDLPL_PLUS_HLP_PSC*17
      PARAMETER  ( HFEOP_CMP_DEF      = 'jmg96.hf'          )
      PARAMETER  ( MDLPL_EXT_HLPFIL   = 'mdlpl_ext.hlp'     )
      PARAMETER  ( MDLPL_PLUS_HLP_OVR = 'mdlpl_plus_01.hlp' ) ! Overview
      PARAMETER  ( MDLPL_PLUS_HLP_ATM = 'mdlpl_plus_02.hlp' ) ! Atmosphere
      PARAMETER  ( MDLPL_PLUS_HLP_CLO = 'mdlpl_plus_03.hlp' ) ! Clock
      PARAMETER  ( MDLPL_PLUS_HLP_EOP = 'mdlpl_plus_04.hlp' ) ! EOP
      PARAMETER  ( MDLPL_PLUS_HLP_PSF = 'mdlpl_plus_05.hlp' ) ! Post-fit res.
      PARAMETER  ( MDLPL_PLUS_HLP_PSC = 'mdlpl_plus_06.hlp' ) ! Res. + clocks
!
!    MDLFL SPECS:
!    Contains information used to plot recovered valuse of atmosphere
!    and clock from auto-constrained SOLVE.  Essentially al long table
!    of values to be plotted.  Each row corresponds to one time for one
!    site.
!
!     Name         Use
!     ICOUNT       Number of row loaded in the table.
!     TAB_LEN      Maximum length of the table.
!     SITE_ID      SOLVE internal station number for this row.
!     TIME_TAG     YMDHM time tag for this row
!     ATM_VAL      value of estimated zenith path delay (ps)
!     ATM_SIG      sigma of estimated zenith path delay (ps)
!     CLK_VAL      value of estimated clock for this site (ps)
!     CLK_SIG      sigma of estimated clock for this site (ps)
!     press        Pressure used to compute Sasstomoine zenith delay.
!     tempc        Temperature used to compute Sasstomoine zenith delay.
!     humid        Humidityused to compute Sasstomoine wet zenith delay.
!     saszdry      Sasstomoine dry zenith path delay (ps)
!     saszwet      Sasstomoine wet zenith path delay (ps)
!     saszuse      Flag of usage of Sasstamoinen zenith path delay. TRUE means
!                  that it was used in caimputation of a priori zenith
!                  path delay
!     Elevation    elevation at this site(radians)
!     BTC          adjustment vetcor to use to get total clock
!     DEFAULT_A_SCALE - controls whether defaults atm scale is used.
!     DEFAULT_C_SCALE - controls whether defaults clk scale is used.
!     SET_SCALE_INT_A - controls whether to interact for each atm plot.
!     SET_SCALE_INT_C - controls whether to interact for each clk plot.
!     ALT_PLOT_LU     - alturnate plot lu, v.g. the laser printer
!     SAVE_PLOT_FILES - controls whether to save the plot files
!
!     Also will now do earth orientation plotting.  Additional variables:
!
!     IEO_CT        Counts through the list of earth orientation values/sigmas
!                   Each element in list represents a new observation time.
!     EOP_TAG       ymdhm time tag for eo_val's.
!     EO_VAL        List of earth orientation values to be plotted.  Each
!                   row has x,y and UT1 for a given obs time
!
!     EO_SIG        List of earth orientation sigmas to be plotted.
!
!     IEO_TIME_PT   List of pointers to elements in the atmosphere and clock
!                   lists; used to look up the time of the observation being
!                   plotted, rather than duplicating a subset of the time_tag
!                   array for the earth orientation parameters' use
