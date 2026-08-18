!     This is the start of skedh.i, the include file for the sskedh program
!
!     :93.04.08: kdb created for new sskedh program
!     :94.01.03: kdb Add db_exper variable to hold onto the experiment name
!                    read from the input file's $EXPER card until it can be
!                    displayed to the user in another subroutine.
!     :94.04.14: kdb Bring up to date with latest version of SOLVE (first
!                    export version):  New variables: source_name, source_cood,
!     :95.02.16: KDB Added option to more closely simulate the database made
!                    from the experiment actually run from the input schedule
!                    (in terms of which observations are used in the solution).
!     95.09.28:  KDB  Move max_num_met_values to solve.i
!     951207     kdb  Integer*4 number of observations.
!     96.03.27:  KDB  Upgrade sskedh for 32 sites.  (Set max_local_sites to
!                     max_arc_sta.)
!     961107     kdb  Raise flaval_names array size from 15 to 112.
!     2004.10.15 pet  Raised max_local_stars from 300 to 16384 
!
      INTEGER*2   MAX_LOCAL_SITES, MAX_LOCAL_STARS, MAX_LOCAL_BASELINES
      PARAMETER ( MAX_LOCAL_SITES = MAX_ARC_STA )
      PARAMETER ( MAX_LOCAL_STARS = 16384 )
      PARAMETER ( MAX_LOCAL_BASELINES = 120 )
!
      REAL*8 &
     &                               IPOCH          , &
     & WAXOF(MAX_LOCAL_SITES), wSITEC(3,MAX_LOCAL_SITES), &
     & WSTARC(2,MAX_LOCAL_STARS),    WAMB, &
     & WATM          ,WREL          ,WCOR , WTIDE(3)      , &
     & WPREC         ,WNUT          ,TMED(2,10), &
     & FEPOC         ,FCLJD         ,FCLRC, &
     & TMIN          ,TMAX          ,ECC(3,MAX_LOCAL_SITES), &
     & ECC_ROT(3,MAX_LOCAL_SITES), &
     & ACCLM, FK(4,MAX_LOCAL_BASELINES), SNRLM, &
     & BARO_CAL_EPOCHS(2,MAX_LOCAL_SITES), &
     & BARO_CAL_VALUES(2,MAX_LOCAL_SITES), &
     & MAX_TEMP(MAX_LOCAL_SITES), MIN_TEMP(MAX_LOCAL_SITES), &
     & AVG_TEMP(MAX_LOCAL_SITES), RMS_TEMP(MAX_LOCAL_SITES), &
     & MAX_PRES(MAX_LOCAL_SITES), MIN_PRES(MAX_LOCAL_SITES), &
     & AVG_PRES(MAX_LOCAL_SITES), RMS_PRES(MAX_LOCAL_SITES), &
     & TEMPERATURE(MAX_NUM_MET_VALUES,MAX_LOCAL_SITES), &
     & PRESSURE(MAX_NUM_MET_VALUES,MAX_LOCAL_SITES), &
     & BREAK_EPOCS(MAX_LOCAL_SITES),  SOURCE_COOD(MAX_LOCAL_STARS), &
     & SITE_ALPHA(MAX_LOCAL_SITES), &
     & DBM_ELMIN, DBM_ELVCUT(MAX_ARC_STA), DBM_TIMES(MAX_OBS), &
     & DBM_GRDEL_SIGS(MAX_OBS), DBM_GIONSG(2,MAX_OBS), DBM_BP(3,2,MAX_OBS), &
     & DBM_SP(2,MAX_OBS), DBM_NUTP(2,MAX_OBS), DBM_ROTP(3,MAX_OBS), &
     & DBM_AZ(2,MAX_OBS), DBM_ELEV(2,MAX_OBS), DBM_AP(2,MAX_OBS)
!
      INTEGER*2 &
     & OBCAPL        ,OBCAVL        ,OBTHEO       , &
     & MONU_NAME(5,MAX_LOCAL_SITES), &
     & MONU_TYPE(MAX_LOCAL_SITES), IBSTB(4,2,MAX_LOCAL_BASELINES), &
     & IPASS(53), &
     & IDATE(4),IDBN, IDIFLG, &
     & IGETT,IION, INIT, &
     & INM(5),INSOLUT(8), IOBSTAND, IREST, &
     & IRETN, IRM(5), ISDIF(MAX_LOCAL_SITES), &
     & ISISUBS, ISOSUBS(5), ISTAND, &
     & ISTFL(MAX_LOCAL_SITES), ISTFLG, ISTRED(96), &
     & ITMFLG, IXORS, JCAVAL(MAX_LOCAL_SITES), JCORR, &
     & JCZEN(MAX_LOCAL_SITES), JION, JNCAL, &
     & JNSTR, JSITI(MAX_LOCAL_SITES), &
     & JSITN(4,MAX_LOCAL_SITES), &
     & IBLED(2,60), JNSTA, IBLFLG, IFIRST, &
     & ISLVEB, &
     & JSTRN(4,MAX_LOCAL_STARS), LETRS, LFACT(2,10), &
     & LMACT, R_TO_Z_TAB(10), NCORR, NFLBL, NNVER, NOBCAL, &
     & NSTA, NPAGE, NSTR, NUMPLT, &
     & JCAPPL(MAX_LOCAL_SITES), JCAFFL(7,MAX_LOCAL_SITES), &
     & IRETDBERR, IDELET, NFCAL, NZCAL, &
     & Z_TO_R_TAB(8), NUM_MET_VALS_BY_SITE(MAX_LOCAL_SITES), &
     & LOCAL_MET_SITES_USED,   NUMBREAKS,  BREAK_SITES(MAX_LOCAL_SITES), &
     & BREAK_FLAGS(MAX_LOCAL_SITES),  NFLAVAL, &
     & LU_AUSK, &
     & SOURCE_NAME_I2(4,MAX_LOCAL_STARS),SITE_NAME_I2(4,MAX_LOCAL_SITES), &
     & DBM_WEIGHTS(MAX_OBS), &
     & DBM_NFAIL_SITES,DBM_NSUB_SITES, &
     & DBM_JSITI(MAX_LOCAL_SITES)
!
      INTEGER*4 LIN_AUSK, NUMOS, DBM_NOBS
!
      LOGICAL*2 &
     & KOBTHEO, LEVER, LPHAS, &
     & KSUPER, KCURNT, KABORT, &
     & LIPTON, DOMPSUB, &
     & BARO_CAL_STATUS(MAX_LOCAL_SITES), BARO_CAL_AVAIL, &
     & DBM_SITE_DEP_EL_CUT, DBM_SET_ELMIN, DBM_ST_ELVCUT
	CHARACTER  JSITN_CHR(MAX_LOCAL_SITES)*8
        EQUIVALENCE ( JSITN, JSITN_CHR )
!
      CHARACTER*1 DBM_FLAG,DBM_FILLER
!
      CHARACTER*8 &
     & QDCAL(15), QLCAL(15),         QSITN(MAX_LOCAL_SITES), &
     & QDOBCAL(15), QLOBCAL(15),     QDFCAL(112),            &
     & MET_SITES(MAX_LOCAL_SITES),   FLAVAL_NAMES(112),       &
     & SOURCE_NAME(MAX_LOCAL_STARS), SITE_NAME(MAX_LOCAL_SITES), &
     & DBM_FAIL_SITES(MAX_ARC_STA),  DBM_BLINES(2,MAX_OBS), &
     & DBM_SOURCES(MAX_OBS),         DBM_SUB_SITES(2,MAX_ARC_STA)
!
      CHARACTER*10 DB_EXPER
!
!
      CHARACTER    QDZCAL(15)*16 
      CHARACTER    JSTRN_CHR(MAX_LOCAL_STARS)*8
      EQUIVALENCE  ( JSTRN, JSTRN_CHR )
!
      CHARACTER    NEXTSUB*6 
      CHARACTER    INPUT_PATH*64 
      CHARACTER    NAME_USED_SOURCE(MAX_ARC_SRC)*8
      INTEGER*4    NUM_USED_SOURCE
!
      EQUIVALENCE ( JSITN(1,1), QSITN(1) )
      EQUIVALENCE ( SOURCE_NAME,SOURCE_NAME_I2)
      EQUIVALENCE ( SITE_NAME, SITE_NAME_I2 )
!
      COMMON /KSDC/ &
     & IPOCH, &
     & WAXOF, WSITEC, WSTARC, WAMB, &
     & WATM,  WREL, WCOR, WTIDE, &
     & WPREC, WNUT, TMED, &
     & FEPOC, FCLJD, FCLRC, &
     & TMIN,  TMAX, ECC, ECC_ROT, &
     & ACCLM, FK,SNRLM, BARO_CAL_EPOCHS, BARO_CAL_VALUES , &
!
     & OBCAPL, OBCAVL, OBTHEO, MONU_NAME,  &
     & MONU_TYPE, IBSTB, IPASS,  &
     & IDATE, IDBN, IDIFLG,  &
     & IGETT, IION, INIT,  &
     & INM, INSOLUT, IOBSTAND, IREST,  &
     & IRETN, IRM, ISDIF,  &
     & ISISUBS, ISOSUBS, ISTAND,  &
     & ISTFL, ISTFLG, ISTRED,  &
     & ITMFLG, IXORS, JCAVAL, JCORR,  &
     & JCZEN, JION, JNCAL,  &
     & JNSTR, JSITI,  &
     & JSITN,  &
     & IBLED, JNSTA, IBLFLG, IFIRST,  &
     & ISLVEB,  &
     & JSTRN, LETRS, LFACT,  &
     & LMACT, R_TO_Z_TAB,  &
     & NCORR, NFLBL, NNVER, NOBCAL       ,  &
     & NSTA, NPAGE,  &
     & NSTR, NUMOS, NUMPLT,  &
     & JCAPPL, JCAFFL,  &
     & IRETDBERR, IDELET, NFCAL, NZCAL,  &
     & Z_TO_R_TAB, LIN_AUSK, LU_AUSK, &
!
     & KOBTHEO,LEVER,LPHAS, &
     & KSUPER,KCURNT,KABORT, &
     & LIPTON ,DOMPSUB, &
     & BARO_CAL_STATUS,BARO_CAL_AVAIL, &
!
     & QDCAL,QLCAL,QDOBCAL, &
     & QLOBCAL,QDFCAL,QDZCAL, &
!
     & INPUT_PATH, &
!
     & NEXTSUB, &
!
     & MAX_TEMP, MIN_TEMP, AVG_TEMP, RMS_TEMP, &
     & MAX_PRES, MIN_PRES, AVG_PRES, RMS_PRES, &
     & MET_SITES, LOCAL_MET_SITES_USED, &
     & TEMPERATURE, PRESSURE, NUM_MET_VALS_BY_SITE, &
     & NUMBREAKS, BREAK_EPOCS, BREAK_SITES, BREAK_FLAGS, &
     & NFLAVAL,FLAVAL_NAMES, DB_EXPER, SOURCE_COOD, SOURCE_NAME, &
     & SITE_ALPHA, SITE_NAME, &
     & DBM_ELMIN, DBM_ELVCUT, DBM_TIMES, DBM_GRDEL_SIGS, DBM_GIONSG, &
     & DBM_BP, DBM_SP, DBM_NUTP, DBM_ROTP, DBM_AZ, DBM_ELEV, DBM_AP, &
     & DBM_BLINES, DBM_SOURCES, DBM_WEIGHTS, DBM_NOBS, &
     & DBM_NFAIL_SITES, DBM_NSUB_SITES, &
     & DBM_JSITI, DBM_SITE_DEP_EL_CUT, DBM_SET_ELMIN, &
     & DBM_st_elvcut, DBM_FAIL_SITES, DBM_SUB_SITES, DBM_FLAG, DBM_FILLER, &
     & NUM_USED_SOURCE, NAME_USED_SOURCE
