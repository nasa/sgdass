!@This is the start of file &KSDC
!
!     *********************  NOTE  ************************
!
!     This is the new version of KSDC -- it consists of old
!     KSDC merged with the variables in SDCOM worth keeping.
!
!     :88.12.15: jwr Real*6 to Real*8 for unix conversion
!     :91.10.04: jwr Max, Min, average, rms temperature and pressure
!                    variables added.
!     :91.10.15  aee for jwr, changed max_num_met_values from 400 to 600.
!     :92.12.30:jwr: isitn_bc and numsta_bc added to salvage baseline
!                    dependent clock logic in blkcl.
!     :95.05.22:jwr: Local parameters replace with parameters from solve.i
!     :95.09.28:kdb: Move max_num_met_values to solve.i.
!     :95.12.04:kdb: Allow integer*4 number of observations.
!     :96.05.01:jwr: Clock epoch array dimensions changed from MAX_ARC_STA
!                    to MAX_CLK.
!     :98.03.26:pet: Added l_ACM, STAT_ACM, CLOOF_ACM, CLODR_ACM
!    1999.10.14 pet  Added MONU_NAME_STR, MONU_TYPE_STR and equivalenced them
!                    to MONU_NAME and MONU_TYPE respectively.
!    1999.11.16 pet  Added MCAVL, MCAPL, MCALNAMS, MCAL_LCODE, J4_MCAL,
!                    MCAL_DEF, MCAL_IN_DBASE
!    2000.06.14 pet  Added LEN_RFREQ_R8, LEN_SCANNAME_I2, LEN_RUNCODE_I2,
!                    LEN_EXP_NAME_I2, LEN_PI_NAME_I2, LEN_CORRELATOR_NAME_I2,
!                    LEN_VLBI_MODE_I2
!    2000.07.04 pet  Added FRQGROUP_CH, LEM_RECMODE_I2
!    2001.07.10 pet  Added APLENGTH
!
      REAL*8 &
     &                              IPOCH          , &
     &WAXOF(max_arc_sta),WSITEC(3,max_arc_sta), &
     &WSTARC(2,max_arc_src)  ,WAMB  , &
     &WATM          ,WREL          ,WCOR           ,WTIDE(3)      , &
     &WPREC         ,WNUT          ,TMED(2,10)     , &
     &FEPOC         ,FCLJD         ,FCLRC          , &
     &TMIN          ,TMAX          ,ECC(3,max_arc_sta) , &
     &ECC_ROT(3,max_arc_sta), &
     &ACCLM         ,FK(4,max_plots)               ,SNRLM          , &
     &BARO_CAL_EPOCHS(2,max_arc_sta)        , &
     &BARO_CAL_VALUES(2,max_arc_sta), &
     &Max_temp(max_arc_sta),            Min_temp(max_arc_sta), &
     &Avg_temp(max_arc_sta),            Rms_temp(max_arc_sta), &
     &Max_pres(max_arc_sta),            Min_pres(max_arc_sta), &
     &Avg_pres(max_arc_sta),            Rms_pres(max_arc_sta), &
     &temperature(max_num_met_values,max_arc_sta), &
     &pressure   (max_num_met_values,max_arc_sta), &
     &break_epocs(max_clk)
!
      INTEGER*2 &
     &OBCAPL        ,OBCAVL        ,OBTHEO       , &
     &MONU_NAME(5,max_arc_sta), &
     &MONU_TYPE(max_arc_sta) ,IBSTB(4,2,max_plots) , &
     &IPASS(53)     , &
     &IDATE(4)      ,IDBN          ,IDIFLG         , &
     &IGETT         ,IION          ,INIT           , &
     &INM(5) ,INSOLUT(max_arc_bsl_words) ,IOBSTAND  ,IREST     , &
     &IRETN         ,IRM(5)        ,ISDIF(max_arc_sta)      , &
     &ISISUBS       ,ISOSUBS(5)    ,ISTAND         , &
     &ISTFL(max_arc_sta)       ,ISTFLG         ,ISTRED(96)     , &
     &ITMFLG        ,IXORS         ,JCAVAL(max_arc_sta)     ,JCORR, &
     &JCZEN(max_arc_sta)                    ,JION           ,JNCAL, &
     &JNSTR         ,JSITI(max_arc_sta)     , &
     &JSITN(4,max_arc_sta)   , &
     &IBLED(2,60)   ,JNSTA         ,IBLFLG         ,IFIRST       , &
     &ISLVEB        , &
     &JSTRN(4,max_arc_src)  ,LETRS         ,LFACT(2,MAX_CAL)    , &
     &LMACT         ,R_TO_Z_TAB(MAX_CLZ), &
     &NCORR         ,NFLBL         ,NNVER          ,NOBCAL       , &
     &NSTA          ,NPAGE         , &
     &NSTR          ,               NUMPLT         , &
     &JCAPPL(max_arc_sta)      ,JCAFFL(7,max_arc_sta)    , &
     &IRETDBERR     ,IDELET        ,NFCAL          ,NZCAL        , &
     &Z_TO_R_TAB(MAX_CLZ) ,num_met_vals_by_site(max_arc_sta), &
     &local_met_sites_used,   numbreaks,  break_sites(max_clk), &
     &break_flags(max_clk),  nflaval       , &
     &isitn_bc(4,max_arc_sta) , numsta_bc, &
     & MCAVL, MCAPL, MCAL_DEF
!
      LOGICAL*2 &
     &KOBTHEO       ,LEVER          ,LPHAS         , &
     &KSUPER        ,KCURNT         ,KABORT        , &
     &LIPTON        ,DOMPSUB        , &
     &BARO_CAL_STATUS(max_arc_sta)           ,BARO_CAL_AVAIL, &
     & MCAL_IN_DBASE
!
      CHARACTER*8 &
     &QDCAL(MAX_CAL)     ,QLCAL(MAX_CAL)      ,QSITN(max_arc_sta)     , &
     &QDOBCAL(MAX_CONT)    ,QLOBCAL(MAX_CONT)     ,QDFCAL(112)        , &
     &met_sites(max_arc_sta)    ,flaval_names(MAX_CAL), &
     &jsitn_chr(max_arc_sta)    ,jstrn_chr(max_arc_src), &
     &names4errors(2,max_plots) ,isitn_bc_chr(max_arc_sta)
      CHARACTER  IXORS_CHR*2
!
      equivalence (jsitn,jsitn_chr)
      equivalence (jstrn,jstrn_chr)
      equivalence (IBSTB,names4errors)
      equivalence (isitn_bc,isitn_bc_chr)
!
      CHARACTER*16 QDZCAL(MAX_CLZ)
      CHARACTER    MCALNAMS(MM_CLM)*8, MCAL_LCODE(MM_CLM)*8
!
      CHARACTER*6 NEXTSUB
!
      EQUIVALENCE (JSITN(1,1), QSITN(1))
      EQUIVALENCE ( IXORS, IXORS_CHR )
!
      real*8      source_cood   (  max_arc_src)
      character*8 source_name   (  max_arc_src)
      integer *2  source_name_i2(4,max_arc_src)
      equivalence (source_name,source_name_i2)
!
      real*8      site_alpha(  max_arc_sta)
      character*8 site_name     (  max_arc_sta)
      integer *2  site_name_i2  (4,max_arc_sta)
      equivalence (site_name,site_name_i2)
      character*8 ibstb_chr(2,max_arc_sta)
      equivalence (ibstb,ibstb_chr)
      integer*4 site_counter(2,max_arc_sta)
      integer*4 numos
      CHARACTER  FRQGROUP_CH*2
      INTEGER*2 LEN_RFREQ_R8, LEN_SCANNAME_I2, LEN_RUNCODE_I2, &
     &          LEN_EXP_NAME_I2, LEN_PI_NAME_I2, &
     &          LEN_CORRELATOR_NAME_I2, LEN_EXP_DESC_I2, &
     &          LEN_CORRTYPE_I2, LEN_PHCAL_STAT_I2, LEN_RECMODE_I2
!
      INTEGER*4  L_ACM, STAIND_ACM(M_ACM), J4_MCAL
      REAL*8     CLOOF_ACM(M_ACM), CLODR_ACM(M_ACM)
      REAL*8     APLENGTH
!
      Common /KSDC/ &
     &                              IPOCH          , &
     &WAXOF         ,WSITEC        ,WSTARC         ,WAMB         , &
     &WATM          ,WREL          ,WCOR           ,WTIDE        , &
     &WPREC         ,WNUT          ,TMED           , &
     &FEPOC         ,FCLJD         ,FCLRC          , &
     &TMIN          ,TMAX          ,ECC            ,ECC_ROT      , &
     &ACCLM         ,FK            ,SNRLM          , &
     &BARO_CAL_EPOCHS              ,BARO_CAL_VALUES      , &
!
     &OBCAPL        ,OBCAVL        ,OBTHEO         ,MONU_NAME    , &
     &MONU_TYPE     ,IBSTB         ,IPASS          , &
     &IDATE         ,IDBN          ,IDIFLG         , &
     &IGETT         ,IION          ,INIT           , &
     &INM           ,INSOLUT       ,IOBSTAND       ,IREST        , &
     &IRETN         ,IRM           ,ISDIF          , &
     &ISISUBS       ,ISOSUBS       ,ISTAND         , &
     &ISTFL         ,ISTFLG        ,ISTRED         , &
     &ITMFLG        ,IXORS         ,JCAVAL         ,JCORR        , &
     &JCZEN                        ,JION           ,JNCAL        , &
     &JNSTR         ,JSITI         , &
     &JSITN         , &
     &IBLED         ,JNSTA         ,IBLFLG         ,IFIRST       , &
     &ISLVEB        , &
     &JSTRN         ,LETRS         ,LFACT          , &
     &LMACT         ,R_TO_Z_TAB    , &
     &NCORR         ,NFLBL         ,NNVER          ,NOBCAL       , &
     &NSTA          ,NPAGE         , &
     &NSTR          ,NUMOS         ,NUMPLT         , &
     &JCAPPL        ,JCAFFL        , &
     &IRETDBERR     ,IDELET        ,NFCAL          ,NZCAL        , &
     &Z_TO_R_TAB    , &
!
     &KOBTHEO       ,LEVER          ,LPHAS         , &
     &KSUPER        ,KCURNT         ,KABORT        , &
     &LIPTON        ,DOMPSUB        , &
     &BARO_CAL_STATUS               ,BARO_CAL_AVAIL, &
!
     &QDCAL         ,QLCAL          ,QDOBCAL      , &
     &QLOBCAL       ,QDFCAL         ,QDZCAL       , &
!
     &NEXTSUB, &
!
     &max_temp, Min_temp, avg_temp, rms_temp, &
     &max_pres, Min_pres, avg_pres, rms_pres, &
     &met_sites, local_met_sites_used, &
     &temperature, pressure, num_met_vals_by_site, &
     &numbreaks, break_epocs,break_sites,break_flags, &
     &nflaval,flaval_names, source_cood, source_name, &
     &site_alpha,site_name, site_counter,isitn_bc,numsta_bc, &
     &  MCAL_DEF, &
     &  MCAVL, MCAPL, &
     &  L_ACM, STAIND_ACM, CLOOF_ACM, CLODR_ACM, J4_MCAL, &
     &  MCALNAMS, MCAL_LCODE, &
     &  MCAL_IN_DBASE, &
     &  LEN_RFREQ_R8, &
     &  LEN_SCANNAME_I2, &
     &  LEN_RUNCODE_I2, &
     &  LEN_EXP_NAME_I2, &
     &  LEN_PI_NAME_I2, &
     &  LEN_CORRELATOR_NAME_I2, &
     &  LEN_EXP_DESC_I2, &
     &  LEN_CORRTYPE_I2, &
     &  LEN_PHCAL_STAT_I2, &
     &  FRQGROUP_CH, &
     &  LEN_RECMODE_I2, &
     &  APLENGTH
!CCCC
        CHARACTER     MONU_NAME_CHR(MAX_ARC_STA)*10, &
     &                MONU_TYPE_CHR(MAX_ARC_STA)*2
!
        EQUIVALENCE ( MONU_NAME, MONU_NAME_CHR )
        EQUIVALENCE ( MONU_TYPE, MONU_TYPE_CHR )
