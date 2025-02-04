!@This is the start of file Q_SOCOM
!
! this file is intended to be a clone of SOCOM with Q_
! in front of every name
!
! kdb 1995.12.04  Allow integer*4 number of observations
! pet 2000.06.13  Massive update
! pet 2003.08.15  Added Q_NPAR_GRAD_STA
! pet 2003.08.15  Added Q_NPARAM_AFTER_BASCL 
!
       INTEGER*2   Q_IFREE_LEN
!
       PARAMETER ( Q_IFREE_LEN            =  10 ) ! 2-bytes words
!!
       REAL*8 &
     &            Q_PI_VAR, &
     &            Q_FJDCL(MAX_CLK), &
     &            Q_TATM(MAX_ATM), &
     &            Q_ELMIN, &
     &            Q_VLIGHT, &
     &            Q_TROT(MAX_ROT), &
     &            Q_WRMS(3), &
     &            Q_ROTAP(MAX_ROT,4), &
     &            Q_UT1INB(3), &
     &            Q_WOBINB(3), &
     &            Q_CALCV, &
     &            Q_ATMOS_INTERVAL, &
     &            Q_CLOCK_INTERVAL, &
     &            Q_ROT_INTERVAL(2), &
     &            Q_TROT_A1, &
     &            Q_UT1PTB(MAX_EROT_VALUES), &
     &            Q_WOBXXB(MAX_EROT_VALUES), &
     &            Q_WOBYYB(MAX_EROT_VALUES), &
     &            Q_FCNPER, &
     &            Q_SACNST(MAX_ARC_STA), &
     &            Q_SCCNST(MAX_ARC_STA), &
     &            Q_ELVCUT(MAX_ARC_STA), &
     &            Q_EOPCONS(3), &
     &            Q_EOPRCONS(3), &
     &            Q_SEOCNST(2), &
     &            Q_PWCCNST, &
     &            Q_NUTCONS(2), &
     &            Q_TGRAD(MAX_GRAD), &
     &            Q_GRAD_INTERVAL, &
     &            Q_GRADCONS(2), &
     &            Q_NUTPSI_DIF, &
     &            Q_NUTEPS_DIF, &
     &            Q_NUTPSI_AVE, &
     &            Q_NUTEPS_AVE, &
     &            Q_CHISQR(3),  &
     &            Q_UTC_M_TAI,  &
     &            Q_NUT_XY_AVE(2), &
     &            Q_MEAN_EFF_FREQ(6), &
     &            Q_IOS_SIG
!
       LOGICAL*2 &
     &            Q_LOGBCL, &
     &            Q_BMODE_CL, &
     &            Q_BMODE_AT, &
     &            Q_CLK_BRK_STAT, &
     &            Q_FLYBY_WARNING, &
     &            Q_SITE_DEP_CONST, &
     &            Q_SIMULATION_TEST, &
     &            Q_SITE_DEP_EL_CUT, &
     &            Q_SHORT_UT1_IN, &
     &            Q_SOL_AVAIL, &
     &            Q_OLD_CLOCKS, &
     &            Q_OLD_ATMS, &
     &            Q_SKIP_EOP_OFF, &
     &            Q_CGM_TYPE, &
     &            Q_META_SUP
!
       CHARACTER  Q_USER_PRO*50
       CHARACTER  Q_USER_BUF*68
       CHARACTER  Q_SCR_FIL_ORIGIN*4
       CHARACTER  Q_EXP_DESC*80
       CHARACTER  Q_EXP_CODE*8
       CHARACTER  Q_PI_NAME*80
       CHARACTER  Q_CORRELATOR_NAME*32
       CHARACTER  Q_CORRTYPE*8
       CHARACTER  Q_REC_MODE*80
!
       CHARACTER  Q_UT1_RS*1
       CHARACTER  Q_UT1_RS_FLYBY*1
       CHARACTER  Q_EOP_TS_CALC*8
       CHARACTER  Q_EOP_TS_MODF*8
       CHARACTER  Q_DBNAME_CH*10
       CHARACTER  Q_MK3_DBNM*10
       CHARACTER  Q_ENV_FINAM*128
       CHARACTER  Q_PIMA_CNT*128
       CHARACTER  Q_BAND_NAM(MAX_BND)*1
!
       INTEGER*2 &
     &           Q_NUMSTR, &
     &           Q_NUMSTA, &
     &           Q_NPOLD, &
     &           Q_ICLOCK(ARC_STA_BIT_WORDS,MAX_ARC_STA), &
     &           Q_ICLOCK_P(ARC_STA_BIT_WORDS,MAX_ARC_STA), &
     &           Q_IDNWT, &
     &           Q_IPRES, &
     &           Q_IRNCD(2), &
     &           Q_ITDGLB, &
     &           Q_NPARAM_I2, &
     &           Q_IDATYP, &
     &           Q_NROT, &
     &           Q_NSOURC, &
     &           Q_NSPARM(MAX_ARC_STA), &
     &           Q_NUMATM(MAX_ARC_STA), &
     &           Q_NUMGRAD(MAX_ARC_STA), &
     &           Q_IATSTR(MAX_ARC_STA), &
     &           Q_ICLMAX, &
     &           Q_NUMCLK(MAX_ARC_STA), &
     &           Q_ICLSTR(MAX_ARC_STA), &
     &           Q_IPSTP, &
     &           Q_LNUT(3), &
     &           Q_LPREC, &
     &           Q_LTIDE(STA_BIT_WORDS,3), &
     &           Q_LREL, &
     &           Q_LROT(ROT_BIT_WORDS,3), &
     &           Q_LATM(ATM_BIT_WORDS,3), &
     &           Q_LCLK(MAX_CLK), &
     &           Q_LSTAR(SRC_BIT_WORDS,2), &
     &           Q_LAXOF(STA_BIT_WORDS), &
     &           Q_LSITEC(STA_BIT_WORDS,3), &
     &           Q_ISRSEL(SRC_BIT_WORDS), &
     &           Q_IUEN, &
     &           Q_ICLSTA(ARC_STA_BIT_WORDS,MAX_CLK), &
     &           Q_NFLEPS, &
     &           Q_FLEPS(14), &
     &           Q_NFLPSI, &
     &           Q_FLPSI(14), &
     &           Q_IDPNUT(7), &
     &           Q_NDPNUT, &
     &           Q_LSITEV(STA_BIT_WORDS,3), &
     &           Q_IARCSOC, &
     &           Q_NSLAST, &
     &           Q_IDBSEL, &
     &           Q_NDB, &
     &           Q_IDCSEL, &
     &           Q_IBLSEL_G(ARC_STA_BIT_WORDS,MAX_ARC_STA), &
     &           Q_IBLSEL_P(ARC_STA_BIT_WORDS,MAX_ARC_STA), &
     &           Q_CONSTRAINT_BITS, &
     &           Q_INDL, &
     &           Q_WVMASK(MAX_ARC_STA), &
     &           Q_BM_REF_CL, &
     &           Q_NROT_A1(2), &
     &           Q_EOP_STYLE(2), &
     &           Q_EOPA1_CHOICE(2), &
     &           Q_IEOPL, &
     &           Q_NUMSTAX
      INTEGER*2 &
     &           Q_INTERPOLATION_UT1, &
     &           Q_INTERPOLATION_PM, &
     &           Q_BGROUND, &
     &           Q_LPROP(SRC_BIT_WORDS,2), &
     &           Q_TOTSTA, &
     &           Q_LGRAD(2), &
     &           Q_INIT_INTERACTIVE, &
     &           Q_CLOCK_REF_BITS(ARC_STA_BIT_WORDS), &
     &           Q_IDBEST, &
     &           Q_OPP_STATUS, &
     &           Q_PAMB_STATUS, &
     &           Q_SUPMET, &
     &           Q_EXP_NUM, &
     &           Q_PHCAL_MODE(MAX_ARC_STA), &
     &           Q_PHCAL_MODE_S(MAX_ARC_STA), &
     &           Q_NPAR_GRAD_STA(MAX_ARC_STA), &
     &           Q_NPARAM_AFTER_BASCL_I2, &
     &           Q_EXPSERNO, &
     &           Q_CABLE_SIGN(MAX_ARC_STA), &
     &           Q_FUSED_STATUS, &
     &           Q_NPAR, &
     &           Q_IOS_EST, &
     &           Q_IFREE_SOCOM(Q_IFREE_LEN), &
     &           Q_SOCOM_LAST_I2
!
      INTEGER*4  Q_NUMOBS, &
     &           Q_IDBEND(MAX_DBS), &
     &           Q_NUMSCA, &
     &           Q_DBNAME_VER, &
     &           Q_EDIT_STS, &
     &           Q_NPARAM, &
     &           Q_NPARAM_AFTER_BASCL
      INTEGER*2  Q_STABIT_G(ARC_STA_BIT_WORDS), &
     &           Q_STABIT_P(ARC_STA_BIT_WORDS)
!
      INTEGER*2  Q_ISOCOM(JSOCOM_WORDS)
      INTEGER*4  Q_SOCOM_FIRST_I2
      INTEGER*2  Q_ISO1G_28_33(1908), Q_ISO2G_28_33(23),  Q_ISO3G_28_33(9)
      INTEGER*2  Q_ISO4G_28_33(541),  Q_ISO5G_28_33(136), Q_ISO6G_28_33(496)
!
      EQUIVALENCE ( Q_ISOCOM,         Q_PI_VAR )
      EQUIVALENCE ( Q_SOCOM_FIRST_I2, Q_PI_VAR )
      EQUIVALENCE ( Q_ISO1G_28_33,    Q_PI_VAR )
      EQUIVALENCE ( Q_ISO2G_28_33,    Q_EOPCONS )
      EQUIVALENCE ( Q_ISO3G_28_33,    Q_IDNWT )
      EQUIVALENCE ( Q_ISO4G_28_33,    Q_IPSTP )
      EQUIVALENCE ( Q_ISO5G_28_33,    Q_NFLEPS )
      EQUIVALENCE ( Q_ISO6G_28_33,    Q_ROT_INTERVAL )
!
! --------------------
! |    common        |
! -------------------
!
       Common / SOCOM / &
!
!  real*8
!
     & Q_PI_VAR, Q_FJDCL, Q_TATM, Q_ELMIN, Q_VLIGHT, Q_TROT, Q_WRMS, &
     & Q_ROTAP,  Q_UT1INB,  Q_WOBINB, Q_UT1_RS, Q_UT1_RS_FLYBY, &
     & Q_CALCV,  Q_ATMOS_INTERVAL, Q_CLOCK_INTERVAL, &
     & Q_FCNPER, Q_SACNST, Q_SCCNST, Q_ELVCUT, Q_EOPCONS, &
!
!  logical*2
!
     & Q_LOGBCL, Q_BMODE_CL, Q_BMODE_AT,       Q_CLK_BRK_STAT, &
     & Q_FLYBY_WARNING,      Q_SITE_DEP_CONST, Q_SIMULATION_TEST, &
     & Q_SITE_DEP_EL_CUT,    Q_SHORT_UT1_IN, &
!
!  integer*2
!
     & Q_NUMSTR, Q_NUMSTA, &
!
!  integer*4
!
     & Q_NUMOBS, &

!
!  integer*2
!
     & Q_NPOLD,  Q_ICLOCK,  Q_IDNWT,     Q_IPRES,  &
     & Q_IRNCD,  Q_ITDGLB,  Q_NPARAM_I2, Q_IDATYP, &
     & Q_NROT,   Q_NSOURC,  Q_NSPARM,    Q_NUMATM, &
     & Q_IATSTR, Q_ICLMAX,  Q_NUMCLK,    Q_ICLSTR, &
     & Q_IPSTP,  Q_LNUT,    Q_LPREC,     Q_LTIDE,  &
     & Q_LREL,   Q_LROT,    Q_LATM,      Q_LCLK,   &
     & Q_LSTAR,  Q_LAXOF,   Q_LSITEC,    Q_ISRSEL, &
     & Q_IUEN,   Q_ICLSTA,  Q_NFLEPS,    Q_FLEPS,  &
     & Q_NFLPSI, Q_FLPSI,   Q_IDPNUT,    Q_NDPNUT, &
     & Q_LSITEV, Q_IARCSOC, Q_NSLAST,              &
!
!  integer*4
!
     & Q_IDBEND, &
!
!  integer*2
!
     & Q_IDBSEL,  Q_NDB, Q_IDCSEL, Q_IBLSEL_G, &
     & Q_CONSTRAINT_BITS, Q_INDL, Q_WVMASK, Q_BM_REF_CL, &
!
!    Additions
!
     & Q_EOP_STYLE,   Q_EOPA1_CHOICE,      Q_NROT_A1,          Q_ROT_INTERVAL, &
     & Q_TROT_A1,     Q_SEOCNST,           Q_IEOPL,            Q_NUMSTAX, &
     & Q_SOL_AVAIL,   Q_INTERPOLATION_UT1, Q_INTERPOLATION_PM, Q_BGROUND, &
     & Q_OLD_CLOCKS,  Q_OLD_ATMS,          Q_EOPRCONS,         Q_UT1PTB, &
     & Q_WOBXXB,      Q_WOBYYB,            Q_LPROP,            Q_PWCCNST, &
     & Q_TOTSTA,      Q_SKIP_EOP_OFF,      Q_USER_PRO,         Q_USER_BUF, &
     & Q_NUTCONS,     Q_SCR_FIL_ORIGIN,    Q_GRAD_INTERVAL,    Q_TGRAD, &
     & Q_GRADCONS,    Q_NUMGRAD,           Q_LGRAD,            Q_CLOCK_REF_BITS, &
     & Q_INIT_INTERACTIVE, Q_IDBEST,       Q_NUMSCA,           Q_STABIT_G, &
     & Q_STABIT_P,    Q_IBLSEL_P,          Q_CGM_TYPE,         Q_OPP_STATUS, &
     & Q_PAMB_STATUS, Q_SUPMET,            Q_NUTPSI_DIF,       Q_NUTEPS_DIF, &
     & Q_NUTPSI_AVE,  Q_NUTEPS_AVE,        Q_ICLOCK_P,         Q_EXP_NUM, &
     & Q_PI_NAME,     Q_EXP_DESC,          Q_EXP_CODE,         Q_CORRELATOR_NAME, &
     & Q_CORRTYPE,    Q_PHCAL_MODE,        Q_PHCAL_MODE_S,     Q_REC_MODE, &
     & Q_EOP_TS_CALC, Q_EOP_TS_MODF,       Q_CHISQR,           Q_NPAR_GRAD_STA, &
     & Q_NPARAM_AFTER_BASCL_I2,               Q_DBNAME_VER,       Q_DBNAME_CH, &
     & Q_EXPSERNO,    Q_UTC_M_TAI,         Q_CABLE_SIGN,       Q_EDIT_STS,  &
     & Q_ENV_FINAM,   Q_MK3_DBNM,          Q_META_SUP,         Q_NUT_XY_AVE,  &
     & Q_PIMA_CNT,    Q_BAND_NAM,          Q_FUSED_STATUS,     Q_NPARAM, &
     & Q_NPARAM_AFTER_BASCL, Q_MEAN_EFF_FREQ, Q_IOS_SIG,       Q_IOS_EST, &
     & Q_IFREE_SOCOM, Q_SOCOM_LAST_I2
