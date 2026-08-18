!@This is the start of file &OBORG
!
!  this is a representation of the A-900 OBSFIL record
!
!     BEGIN 'OBSFIL' SPECIFICATION  12/31/85
!
!           REAL*8         A_FJD,      A_FRACT,    A_FRACTC,   A_DT,
      CHARACTER*8          A_FJD,      A_FRACT,    A_FRACTC,   A_DT, &
     &               A_DOBS,     A_RT,       A_ROBS,     A_TOTPH, &
     &               A_DPH,      A_DNB,      A_PHION,    A_PHAMI8, &
     &               A_DPHSB,    A_DOBSXS,   A_DPHXS
!
!           REAL*4         A_FREQ,     A_ELEV(2),  A_AZ(2),    A_FAMB,
      CHARACTER*4          A_FREQ,     A_ELEV(2),  A_AZ(2),    A_FAMB, &
     &               A_DERR,     A_RERR,     A_BP(3,2,2),A_SP(2,2), &
     &               A_AP(2,2),  A_AXOFP(2,2),A_ROTP(3,2),A_RELP(2), &
     &               A_TIDP(3,2,2),A_PRCP(2), &
     &               A_NUTP(2,2),            A_NPPRIN(2,2,2), &
     &               A_NPSEMA(2,2,2),        A_NPSEMM(2,2,2), &
     &               A_NPDECA(2,2,2),        A_NPANNU(2,2,2), &
     &               A_NP122D(2,2,2),        A_SNR, &
     &               A_TEMPC(2), A_ATMPR(2), A_RELHU(2), A_GIONSG(2), &
     &               A_PHIONS,   A_EFFREQ,   A_SECTAG,   A_PCDLY(2), &
     &               A_DNBER,    A_DPHER,    A_DPHSER,   A_GION(2), &
     &               A_DERRXS,   A_DPHERXS, &
     &               A_CALIBS(2,2,10),       A_CALIBZ(2,2,8), &
     &               A_CALIBB(2,6)
!
            INTEGER*2      A_ISITE(2), A_ISTAR,    A_IUNW,     A_IUNWP, &
     &               A_ICNCL,    A_NUMAMB,   A_IPNTR,    A_IONFLG, &
     &               A_NUMDB,    A_IWVCOD(2),A_IFREO(10),A_LQUAL, &
     &               A_ICORR,    A_IWVBIT1(2),A_IWVBIT2(2)
!
            INTEGER*4      A_NPHAM4
!
            INTEGER*2      A_IOBSDC(16),A_IDAT(512)
!
            EQUIVALENCE      (A_IDAT(1),A_ISITE(1))
!
            COMMON/A_OBORG/A_IOBSDC, &
     &                  A_ISITE,           A_ISTAR,           A_FJD, &
     &                  A_FRACT,           A_FRACTC,          A_IUNW, &
     &                  A_IUNWP,           A_FREQ,            A_ELEV, &
     &                  A_AZ,              A_ICNCL,           A_FAMB, &
     &                  A_NUMAMB,          A_IPNTR,           A_DT, &
     &                  A_DOBS,            A_DERR,            A_RT, &
     &                  A_ROBS,            A_RERR,            A_BP, &
     &                  A_SP,              A_AP,              A_AXOFP, &
     &                  A_ROTP,            A_RELP, &
     &                  A_TIDP,            A_PRCP, &
     &                                     A_NUTP,            A_NPPRIN, &
     &                  A_NPSEMA,          A_NPSEMM,          A_NPDECA, &
     &                  A_NPANNU,          A_NP122D, &
     &                  A_TOTPH,           A_SNR,             A_NUMDB, &
     &                  A_TEMPC,           A_ATMPR,           A_RELHU, &
     &                  A_DPH,             A_DNB,             A_DPHER, &
     &                  A_DPHSER, &
     &                  A_DNBER,           A_PHAMI8,          A_NPHAM4, &
     &                  A_DPHSB,           A_LQUAL,           A_GION, &
     &                  A_GIONSG,          A_PHION,           A_PHIONS, &
     &                  A_ICORR, &
     &                  A_EFFREQ,          A_SECTAG, &
     &                  A_PCDLY, &
     &                  A_IONFLG, &
     &                  A_CALIBS,          A_CALIBZ,          A_CALIBB, &
     &                  A_IWVCOD,          A_DOBSXS,          A_DPHXS, &
     &                  A_DERRXS,          A_DPHERXS,         A_IWVBIT1, &
     &                  A_IWVBIT2,         A_IFREO
!
!     END 'OBSFIL' SPECIFICATIONS
