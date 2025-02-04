!@This is the start of file &BA2CM
!
!  Last update  2016.10.21_14:35:55  Leonid Petrov
!
!  Modifications:
!
!  kdb 970204 New site weighting feature.  New variable (weight_type_gen).
!  pet 970515 New variables CLKPOL_FLG and CLKPOL_DEG added.
!  pet 970902 New variables ACCEOP_FLG added.
!  kdb 980223 Install JMG's sinex output feature in batch mode
!             (New variables lsinex and llsnxdir.)
!  pet 980615 Increased length of the varaibles CFNAME, CFNAME_ORIGINAL
!             ( name of the control file ) from 36 to 128 bytes
!  pet 990419 Added variables LENCNT, LENARC
!  pet 2000.03.29  Added variable WEIGHT_ALGORITHM
!  pet 2000.10.05  Corrected a bug: EOPMID should be of INTEGER*2 type!
!  pet 2000.11.24  Added KVELCONST variable
!  pet 2001.08.10  Removed STACMP, SRCCMP, TIDFLG avriables.
!                  Added VELFLG, PROFLG variables.
!  pet 2002.03.27  Removed variables lsinex and llsnxdir
!  pet 2002.12.24  Added variables SIT_EST_EPOCH_VAL, SOU_EST_EPOCH_VAL
!  pet 2006.12.08  Added variable EOP_EPOCH_SEC
!  pet 2016.10.21  Added variable EOP_EPOCH_MJD
!  pet 2021.07.12  Added variables EOP_BEFORE_SEC_TAI, EOP_AFTER_SEC_TAI
!  pet 2022.08.22  Added variable  IOS_EST_BATCH
!
      CHARACTER*1 SOLTYP,STAFLG,SRCFLG,STACRY,SRCCRY,UT1FLG,ATMCNS
      CHARACTER*1 RSTOUT,MINOUT,BASOUT,SCNOUT,SOLTY2,PRCFLG,CLKCNS
      CHARACTER*1 RELFLG,STADIU,ATMFLG, CLKFLG, TBLOUT, CLKPOL_FLG, &
     &            AXSFLG, VELFLG, PROFLG
      CHARACTER*1 EOPCNS,weights,eoprcns,posell,refreq,pwccns,blcflg
      character*1 weight_type_gen
      character*1 basdf,velcns,ionflg,nutcns,gradflg,gradcns
      CHARACTER*2 NUTFLG(116)
      INTEGER*4   MF_WEI, LF_WEI
      PARAMETER  ( MF_WEI = 16 )
      CHARACTER*12 IDBNAME, JDBNAME
      CHARACTER*20 QICOV,QJCOV
      CHARACTER    DBNAME_MES*16
      CHARACTER*(NAME_SIZE) B_ARCDIR(3)
      CHARACTER    WEIGHT_FILE(MF_WEI)*128
      CHARACTER    CFNAME*128, CFNAME_ORIGINAL*128
      CHARACTER*60 ID(10)
      CHARACTER*64 CGMNMR,USER_PROG
      CHARACTER    STRING*128
      CHARACTER    USER_BUFF*81, FIXSTA_CHR*8, FIXSRC_CHR*8
      INTEGER*2    FIXSTA(4), FIXSRC(4), ISTRNG(64), GRINTRVL
      EQUIVALENCE ( FIXSTA, FIXSTA_CHR )
      EQUIVALENCE ( FIXSRC, FIXSRC_CHR )
      INTEGER*2 IPASS,ARCNUM,FWDOUT,ITARCS,INTRVL, CKNTRVL, CLKPOL_DEG, &
     &          IP(5)
      INTEGER*4 ARCREC,ARCPOS,PRGREC,PRGPOS,ETIME0,ETIMP0
      INTEGER*2 QCLKEXCPT(4),QATMEXCPT(4)
      LOGICAL*2 RESTRT, KCORL, B_KPERMARC,SOLARCH_SOL
      REAL*8    FCNPR,QATMCNST(2),QCLKCNST(2),EOPSIG(3), EOPRSIG(3)
      real*8    qpwccnst,velsig(3,3),nutsig(2),qgradcnst(2)
      REAL*8        EOP_BEFORE_SEC_TAI, EOP_AFTER_SEC_TAI
      INTEGER*2 OFFLG, RATFLG, ACCEOP_FLG, IEOP_FLG(6), EOPMID, IVER, &
     &          IOS_EST_BATCH
      real*8    REOP_FLG(4), IOS_SIG_BATCH
      integer*2 ieopl_ba,nesm,nexcbl,iblnm(8,MAX_STA)
      character*8 esmsites(MAX_ESM),sol_tag,incgm_sol,exvelnm(2,4)
      real*8 esmdates(MAX_ESM),eopfact
      CHARACTER*2 USER_TAG, RUN_INITS, INCGM_USER
      INTEGER*2 ITEM_LUS(7),ARC_FILE_VER,ARC_FILE_KEY(5)
      CHARACTER*4 ARC_FILE_TYPE,INCGM_TYPE
      CHARACTER*63 ARC_FILE_PATH
      character*80 resfile
      CHARACTER  PARU_FILE*128 
      logical*2 khfeopcal,khfeopest,kplod,no_superfile,modoutflg
      real*8 posepoch
      INTEGER*2  POSNUM
      LOGICAL*2  KMIN_SIG, KSUB_ARC, KOUTNRM, KZERONRM, KSTACONST, &
     &           KVELCONST
      INTEGER*4  WEIGHT_ALGORITHM, IND_PROC, NUM_PROC, EOP_EPOCH_MJD
      INTEGER*2  LENCNT, LENARC
      REAL*8     SIT_EST_EPOCH_VAL, SOU_EST_EPOCH_VAL, EOP_EPOCH_SEC
!
      COMMON/BA2CM/ WEIGHT_ALGORITHM, &
     &              ARCREC, ARCPOS, ITARCS, FIXSTA, FIXSRC, &
     &              ETIME0, &
     &              ETIMP0, ISTRNG, IP, PRGREC, PRGPOS, RESTRT, ARCNUM, &
     &              FCNPR, KCORL, INTRVL, CLKPOL_DEG, &
     &              CKNTRVL, QATMCNST, &
     &              QCLKCNST, EOPSIG, &
     &              FWDOUT, IPASS, SOLTYP, SOLTY2, CGMNMR, ID, &
     &              STAFLG, STADIU, VELFLG, SRCFLG, PROFLG, &
     &              NUTFLG, UT1FLG, PRCFLG, RELFLG, &
     &              STACRY, SRCCRY, &
     &              RSTOUT, MINOUT, BASOUT, SCNOUT, &
     &              QICOV, QJCOV, IDBNAME, JDBNAME, ATMFLG, CLKPOL_FLG, &
     &              CLKFLG, QCLKEXCPT, QATMEXCPT, &
     &              AXSFLG, CLKCNS, ATMCNS, B_KPERMARC, &
     &              OFFLG, RATFLG, ACCEOP_FLG, IEOP_FLG, REOP_FLG, &
     &              IEOPL_BA, &
     &              NESM, ESMSITES, ESMDATES, EOPRSIG, EOPRCNS, EOPFACT, &
     &              POSELL, REFREQ, PWCCNS, QPWCCNST, BLCFLG, &
     &              NEXCBL, IBLNM, BASDF, RUN_INITS,USER_TAG,SOL_TAG,IVER, &
     &              ITEM_LUS, CFNAME_ORIGINAL, ARC_FILE_TYPE, &
     &              ARC_FILE_PATH, ARC_FILE_KEY, ARC_FILE_VER, &
     &              INCGM_TYPE, INCGM_USER, INCGM_SOL, SOLARCH_SOL, EOPMID, &
     &              VELCNS, VELSIG, EXVELNM, KHFEOPCAL, KHFEOPEST, KPLOD, &
     &              IONFLG, POSEPOCH, POSNUM, NO_SUPERFILE, MODOUTFLG, &
     &              RESFILE, KMIN_SIG, KSUB_ARC, NUTSIG, NUTCNS, &
     &              GRADFLG, GRINTRVL, GRADCNS, QGRADCNST, KOUTNRM, &
     &              KZERONRM, KSTACONST, KVELCONST, DBNAME_MES, &
     &              LENCNT, LENARC, EOP_EPOCH_SEC, EOP_EPOCH_MJD, &
     &              PARU_FILE, IND_PROC, NUM_PROC, &
     &              EOP_BEFORE_SEC_TAI, EOP_AFTER_SEC_TAI, IOS_EST_BATCH, IOS_SIG_BATCH
!
      EQUIVALENCE (ISTRNG,STRING)
!
      COMMON / BA2CMCH / USER_PROG, USER_BUFF, TBLOUT, B_ARCDIR, CFNAME, &
     &                   EOPCNS, WEIGHTS, WEIGHT_FILE, WEIGHT_TYPE_GEN, &
     &                   SIT_EST_EPOCH_VAL, SOU_EST_EPOCH_VAL, LF_WEI
!
