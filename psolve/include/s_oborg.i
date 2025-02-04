!@This is the start of file OBORG
!
!  S_OBORG is defined as a clone of oborg wih S_ prepended.
!  used for S-BAND observations in IONO
!
!     BEGIN 'OBSFIL' SPECIFICATION  89/03/13
!
      REAL*8           S_FJD,      S_FRACT,    S_FRACTC,   S_DT, &
     &                 S_DOBS,     S_RT,       S_ROBS,     S_TOTPH, &
     &                 S_DPH,      S_DNB,      S_PHION,    S_PHAMI8, &
     &                 S_DPHSB,    S_DOBSXS,   S_DPHXS, s_ut1_m_tai
!
      REAL*8           S_FREQ,     S_ELEV(2),  S_AZ(2),    S_FAMB, &
     &                 S_DERR,     S_RERR,     S_BP(3,2,2),S_SP(2,2), &
     &                 S_AP(2,2),  S_AXOFP(2,2),           S_ROTP(3,2), &
     &                 S_RELP(2),  S_TIDP(3,2,2),          S_PRCP(2), &
     &                 S_NUTP(2,2),            S_NPPRIN(2,2,2), &
     &                 S_NPSEMA(2,2,2),        S_NPSEMM(2,2,2), &
     &                 S_NPDECA(2,2,2),        S_NPANNU(2,2,2), &
     &                 S_NP122D(2,2,2),        S_SNR, &
     &                 S_TEMPC(2), S_ATMPR(2), S_RELHU(2), S_GIONSG(2), &
     &                 S_PHIONS,   S_EFFREQ,   S_SECTAG,   S_PCDLY(2), &
     &                 S_DNBER,    S_DPHER,                S_GION(2), &
     &                 S_DERRXS,   S_DPHERXS, &
     &                 S_CALIBS(2,2,15),       S_CALIBZ(2,2,15), &
     &                 S_CALIBB(2,15), &
     &                 s_ut1_m_ut1r , s_y_pole, s_x_pole,s_pheffreq, &
     &                 s_reffreq,  s_effreq_xs, s_pheffreq_xs, &
     &                 s_reffreq_xs, ap_nmf_h(2,2), ap_nmf_w(2,2), &
     &                 s_robsxs, s_rerrxs
!
      INTEGER*4        S_NPHAM4
!
      INTEGER*2        S_ISITE(2), S_ISTAR,    S_IUNW,     S_IUNWP, &
     &                 S_ICNCL,    S_NUMAMB,   S_IPNTR,    S_IONFLG, &
     &                 S_NUMDB,    S_IWVCOD(2),S_LQUAL,    S_ICORR, &
     &                 S_IWVBIT1(2),  S_IWVBIT2(2),s_lqualxs,s_ifill(279)
!
      INTEGER*2        S_IOBSFIL(1536)
!
      EQUIVALENCE (S_IOBSFIL,S_FJD)
!
      SAVE  /S_OBORG/
      COMMON/S_OBORG/ &
!     REAL*8
     &                 S_FJD,      S_FRACT,    S_FRACTC,   S_DT, &
     &                 S_DOBS,     S_RT,       S_ROBS,     S_TOTPH, &
     &                 S_DPH,      S_DNB,      S_PHION,    S_PHAMI8, &
     &                 S_pheffreq, S_reffreq,  S_DPHXS, &
     &                 S_FREQ,     S_ELEV,     S_AZ,       S_FAMB, &
     &                 S_DERR,     S_RERR,     S_BP,       S_SP, &
     &                 S_AP,       S_AXOFP,                S_ROTP, &
     &                 S_RELP,     S_TIDP,     S_PRCP, &
     &                 S_NUTP,                 S_NPPRIN, &
     &                 S_NPSEMA,               S_NPSEMM, &
     &                 S_NPDECA,               S_NPANNU, &
     &                 S_NP122D,               S_SNR, &
     &                 S_TEMPC,    S_ATMPR,    S_RELHU,    S_GIONSG, &
     &                 S_PHIONS,   S_EFFREQ,   S_SECTAG,   S_PCDLY, &
     &                 S_DNBER,    S_DPHER,    s_ut1_m_ut1r, S_GION, &
     &                 S_DERRXS,   S_DPHERXS, &
     &                 S_CALIBS,               S_CALIBZ, &
     &                 S_CALIBB,   s_effreq_xs,s_pheffreq_xs, &
     &                 s_reffreq_xs, ap_nmh_h, ap_nmf_w, robsxs, &
     &                 s_rerrxs, &
!     INTEGER*4
     &                 S_NPHAM4, &
!     INTEGER*2
     &                 S_ISITE,    S_ISTAR,    S_IUNW,     S_IUNWP, &
     &                 S_ICNCL,    S_NUMAMB,   S_IPNTR,    S_IONFLG, &
     &                 S_NUMDB,    S_IWVCOD,   S_LQUAL,    S_lqualxs, &
     &                 s_icorr,    S_IWVBIT1,  S_IWVBIT2, &
     &                 s_ut1_m_tai, s_x_pole,  s_y_pole,   s_ifill
!
!     END 'OBSFIL' SPECIFICATIONS
