!@This is the start of file &NWCOM
!
!   Last modification: 17-NOV-99 17:24:05
!
!   kdb  951207  Integer*4 number of observations.
!   jwr  960501  In clock break arrays MAX_ARC_STA replaced with MAX_CLK
!   pet  971114  Added JCAVLL array
!   pet  971208  Added BASLSTAT_I2 and SOURSTAT_I2 arrays
!   pet  980410  Set dimension of JCAPPL, JCAVLL, JCAFFL  to MAX_ARC_STA
!                istead of previously hard-coded 16
!   pet  1999.11.17  Added cariations MCAVL, MCAPL, L_CLM, QMCAL_LCODES
!
      INTEGER*2 MBSL
      PARAMETER  ( MBSL = MAX_ARC_STA*MAX_ARC_STA )
      REAL*8 FKG(2,max_arc_bsl), FKP(2,max_arc_bsl) , &
     &       ECC(3,MAX_ARC_STA), &
     &       BARO_CALIB(MAX_ARC_STA),BARO_HT(MAX_ARC_STA), &
     &       break_epochs(max_clk)
      INTEGER*2  NAME_BARO(4,MAX_ARC_STA),NUM_FOUND_BARO
      INTEGER*2  BASLSTAT_I2(MBSL), SOURSTAT_I2(MAX_ARC_SRC)
      character*8 qname_baro(max_arc_sta)
      equivalence (name_baro,qname_baro)
!
      INTEGER*2 UPDATE_TYPE,      UPDATE_EPOCHS,    UPDATE_AMB_ED, &
     &          UPDATE_PHASE,     UPDATE_GROUP_ION, UPDATE_PHASE_ION, &
     &          UPDATE_STA_CAL,   UPDATE_OBS_CAL,IBL(4,2,max_arc_bsl), &
     &          LFCI(5),          LFCS(5),        ION_PROG_DATE(4,15), &
     &          LLDAT(4),         LCTYP,            LCNUM, &
     &          LDISP(8),         LCORC(7),         LCSTA(7), &
     &          LCFAC(2),         LLOBCAL(4,6),     NDI(5), &
     &          IAFLAG(MAX_ATM),  LCORF(5),         IVERO, &
     &          CAL_LCODES(4,16), CONT_LCODES(4,16), &
     &          JCAPPL(MAX_ARC_STA),       OBCAPL,           JNSTA, &
     &          IBAS,             LETRS,            IRETURN, &
     &          NUM_SITE_BRK,                       NOBCAL, &
     &          NCAL, &
     &          IVERS,            IVRRX,            IDB, &
     &          JCAVLL(MAX_ARC_STA), MCAVL, MCAPL, L_CLM
      INTEGER*2 &
     &          ITERM,            IRESTART, &
     &          CLK_BRK_SITES(4,max_clk), &
     &          FCAL_NAMES(4,112),NFCAL,  JCAFFL(7,MAX_ARC_STA), &
     &          numbreaks,        break_sites(max_clk), &
     &          break_flags(max_clk)
      integer*4 ibgn,iend
      LOGICAL*2 CLOCK_BREAK
      CHARACTER*10 CLFCI
      CHARACTER*8 LDATE
      CHARACTER*64 QCORF
      INTEGER*2 NAME_ECC(9,   MAX_ARC_STA)
      character*18 qname_ecc( max_arc_sta)
      equivalence (name_ecc,qname_ecc)
      integer*2 ecc_typ(max_arc_sta)
      character*2 qecc_typ(max_arc_sta)
      equivalence (ecc_typ,qecc_typ)
      character*8 ibl_c(2,max_arc_bsl)
      equivalence (ibl,ibl_c)
!
      character*16  blc_baselines(  max_arc_bsl)
      integer*2    iblc_baselines(8,max_arc_bsl)
      EQUIVALENCE (blc_baselines,iblc_baselines)
      integer*2 num_blc_baselines
!
      EQUIVALENCE (LDATE,LLDAT),(CLFCI,LFCI)
      CHARACTER*8 QFCAL_NAMES(112), QCAL_LCODES(16), QCONT_LCODES(16), &
     &            QMCAL_LCODES(16)
      EQUIVALENCE (QFCAL_NAMES(1),FCAL_NAMES(1,1))
      EQUIVALENCE (QCAL_LCODES(1),CAL_LCODES(1,1))
      EQUIVALENCE (QCONT_LCODES(1),CONT_LCODES(1,1))
!
! --- This is the common for the program NEWDB.
!
      COMMON/NWCOM/ &
     &UPDATE_TYPE,      UPDATE_EPOCHS,    UPDATE_AMB_ED, &
     &UPDATE_PHASE,     UPDATE_GROUP_ION, UPDATE_PHASE_ION, &
     &UPDATE_STA_CAL,   UPDATE_OBS_CAL,   LETRS, &
     &IBL,              IRETURN,          LFCI, &
     &LFCS,             IDB,              IVRRX, &
     &IVERS,            IBGN,             IEND, &
     &ION_PROG_DATE,    LDATE,            LCTYP, &
     &LCNUM,            LDISP,            LCORC, &
     &LCSTA,            LCFAC,            LLOBCAL, &
     &NDI,              IAFLAG,           LCORF, &
     &CAL_LCODES,       CONT_LCODES,      QCORF, &
     &IVERO,            NCAL,             NOBCAL, &
     &JCAPPL,           OBCAPL,           FKG, &
     &FKP,              JNSTA,            IBAS, &
     &ITERM,            IRESTART,         CLOCK_BREAK, &
     &CLK_BRK_SITES,       NUM_SITE_BRK, &
     &NAME_ECC,         ECC, &
     &ECC_TYP,          NAME_BARO,        BARO_CALIB, &
     &BARO_HT,          NUM_FOUND_BARO,   NFCAL, &
     &JCAFFL,           FCAL_NAMES    ,   numbreaks, &
     &break_epochs,     break_sites,      break_flags, &
     &blc_baselines,    num_blc_baselines, &
     & JCAVLL,          BASLSTAT_I2,      SOURSTAT_I2, &
     & MCAVL, MCAPL, L_CLM, &
     & QMCAL_LCODES
!
