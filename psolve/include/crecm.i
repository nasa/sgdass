!@This is the start of file &CRECM
!
!  modifications
!
!  kdb 1995.12.07  Integer*4 number of observations
!  pet 1998.04.29  Added ITOTRC_OBS,  ITOTCB_OBS,  ITOTUS_OBS,  ITOTSU_OBS
!  pet 2001.01.11  Added MECHI
!  pet 2002.05.06  Added definition of STAT__STRU
!  pet 2024.07.08  Added NSCA_TOT and NSCA_USED
!
!CCCCC
      INTEGER*2 IBAS(6,MAX_ARC_BSL), ISRC(5,MAX_ARC_SRC)
      INTEGER*2 JSITI(MAX_ARC_STA),  JSITN(4,MAX_ARC_STA)
      INTEGER*2 NLINE, OBCAPL, NDIFF, IBCNT, ISCNT
      INTEGER*2 IMENU, ITT(MAX_ARC_STA), ITTB(MAX_ARC_BSL)
      INTEGER*4 NUMBER, KC, NC
      REAL*8    REWEIGHT_FLOOR, ET(2,MAX_ARC_BSL), SE(MAX_ARC_STA), &
     &          SS(MAX_ARC_SRC)
      LOGICAL*2 GOOD_SITE(MAX_ARC_STA)
!
      REAL*8 BF(3,MAX_ARC_BSL), BW(3,MAX_ARC_BSL)
      REAL*8 SF(3,MAX_ARC_SRC), SW(3,MAX_ARC_SRC)
      REAL*8 FACT(2), WRMSI(2), &
     &       ME_CHI ! Math.expetation of the sum of squares weighted residuals
!
      COMMON / CRECM / JSITI, JSITN, NLINE, OBCAPL, NUMBER, NDIFF, IBCNT,  &
     &                 ISCNT, REWEIGHT_FLOOR, GOOD_SITE, IMENU, ITT, ITTB, & 
     &                 ET, SE, SS
!
      INTEGER*4  ITOTRC_OBS, &   ! Number of potentially recoverable observations
     &           ITOTCG_OBS, &   ! Number of conditionally good observations
     &           ITOTUS_OBS, &   ! Number of used observations
     &           ITOTSU_OBS   ! Number of suppressed observations
      INTEGER*4  ISTAT_SRC(3,MAX_ARC_SRC) ! Source counter statistics
      COMMON / CREMA / IBAS, ISRC, BF, BW, SF, SW, KC, NC, FACT, WRMSI, &
     &                 ME_CHI, ITOTRC_OBS, ITOTCG_OBS, ITOTUS_OBS, ITOTSU_OBS, &
     &                 ISTAT_SRC
!    #
!
! --- Variables for making file of earth orientation adjustments to plot
!
      REAL*8    CTIME_TEST,EOPLOT_CNVRT(3)
      LOGICAL*2 EOPLOT_READY
      INTEGER*2 EOPLOT_CT,EOPLOT_STARTS(3),EOPLOT_NUMS(3)
      INTEGER*2 EOPLOT_TAG(5,100), EOPLOT_DBNAM(5),   EOPLOT_DBVER
      REAL*8    EOPLOT_ADJ(3,100), EOPLOT_SIG(3,100), EOPLOT_TOT(3,100)
      REAL*8    EOPLOT_CTOT(3,100), WEIGHTED_EPOCH(MAX_ARC_SRC), WEIGHT_SUM(MAX_ARC_SRC)
      INTEGER*4  NSCA_TOT(MAX_ARC_SRC), NSCA_USED(MAX_ARC_SRC)
!
      COMMON   / EOPLT / EOPLOT_ADJ, EOPLOT_SIG, CTIME_TEST, EOPLOT_CNVRT, &
     &                   EOPLOT_CT, EOPLOT_STARTS, EOPLOT_NUMS, EOPLOT_TAG, &
     &                   EOPLOT_DBNAM, EOPLOT_DBVER, EOPLOT_READY, EOPLOT_TOT, &
     &                   EOPLOT_CTOT, NSCA_TOT, NSCA_USED, WEIGHTED_EPOCH, WEIGHT_SUM
!
