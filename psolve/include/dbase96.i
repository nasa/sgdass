! "dbase96.i"     Version: 95.09.18 BA
!
!@DBCOM
!
      COMMON /DBCOM/ &
     & LBUF  ,LTOC  ,MTOCL ,MBUFL ,ISPCT ,MAXTC ,NBLK  ,KRUCW ,KIF  , &
     & KOF   ,KSV   ,NEV   ,NOEX  ,NEVR  ,KDRC  ,KDRCL ,KPUT  ,IB   , &
     & IE    ,NV    ,NVDR  ,NWDS  ,NWORDS,NWPV  ,NBACK ,ISTPW ,NVERI, &
     & NVERO ,IDATE ,IADTB ,IADDN ,KSVTD ,NEVTD ,LHS   ,LTC   ,LTE  , &
     & LDR   ,LDE   ,LDT   ,LZZ   ,IDI_un,IDO_un,IDS   ,IDD   ,LNAMI, &
     & LNAMO ,IHIST ,WORDO ,LHBUFF,NCHCK ,MAXEX ,LKOUT1,IVERX ,ICARTO, &
     & ICATOP,NUMTYP,LSTRINGI     ,LSTRINGO     ,IDONE ,PENDING , &
     & OUTPUT_OPENED,ICARTI,input_file   ,output_file  ,LTEXB
!
!  Modifications:
!
!  wordo and ihist have had their places exchanged (lef 3/6/89)
!
!  Path for error message file (dbhem.help) moved to catalog_parameters.i
!   on 1/6/94 by KDB, to centralize parameters.
!
!   BA  95.09.18  Increased size of primary buffer from 2176 to 10000
!                 words.  Also added mbuf32 parameter to express size
!                 (used here and in kai.f).
!
!-----------------------------------------------------------------------
!
!   mbuf32 is the number of 32 byte elements allowed in the primary
!     buffer (i.e. lbuf, rbuf, ibuf, dbuf, jbuf, ltexb).  Current size
!     is "625", i.e. 20000 bytes or 10000 two byte words.  Dbhem.help,
!     message 104, should have matching values.
!
!   :95:12:04:jwr:  toc_length, the number of elements (lcodes) in a
!                   single extent of a table of contents (toc)
!                   introduced so that mtocl could be set as a parameter
!                   and the extent size increased to 1020 elements.
!                   This effectively eliminates the needs for extents.
!                   Also the equivalence between lbuf and ltexb eliminated.
!   2014.06.04  pet Increased mbuf32 to 2500
!
       integer*4 mbuf32
       parameter (MBUF32 = 2500)
!
       integer*4 toc_length
       parameter (toc_length = 1020)
!
       LOGICAL*2 PENDING,OUTPUT_OPENED
       INTEGER*2 &
     & LBUF(MBUF32*16),LTOC(8,toc_length),MTOCL,MBUFL,ISPCT,MAXTC,NBLK, &
     & KRUCW,KIF,KOF,KSV,NEV,NOEX,NEVR,KDRC,KDRCL,KPUT, &
     & IB(5),IE(5),NV(5),NVDR(5),NWDS(5),NWORDS,NWPV(5), &
     & NBACK,ISTPW,NVERI,NVERO,IDATE(5),IADTB(200),IADDN,KSVTD,NEVTD, &
     & LHS,LTC,LTE,LDR,LDE,LDT,LZZ, &
     & IDI_un(16),IDO_un(16),IDS(10),IDD(10),LNAMI(7),LNAMO(7), &
     & IHIST,LHBUFF(40),NCHCK,MAXEX,LKOUT1(5),IVERX,ICARTO,ICATOP, &
     & NUMTYP,IDONE,ICARTI
      REAL*4 WORDO
      Character*157 input_file, output_file
      CHARACTER*19 LSTRINGI
      CHARACTER*25 LSTRINGO
      CHARACTER*6  INAMS   ,INAMD
      INTEGER*4 NEXTS, NEXTD, LASTS, LASTD
      EQUIVALENCE (IDS(3), INAMS),(IDD(3), INAMD)
      EQUIVALENCE (IDS(6), NEXTS),(IDD(6), NEXTD)
      EQUIVALENCE (IDS(8), LASTS),(IDD(8), LASTD)
      REAL*8    RBUF(MBUF32*4)
      REAL*8    DBUF(MBUF32*4)
      INTEGER*4 JBUF(MBUF32*8)
      INTEGER*2 IBUF(MBUF32*16),IDR(10),LTEXB(16,toc_length)
      EQUIVALENCE (LBUF(1)   ,RBUF(1),IBUF(1),DBUF(1),JBUF(1)), &
     &            (IDD(1)    ,IDR(1)                         )
      COMMON /EMA_COMMON/ EMA_BUFFER(40000)
      REAL*4 EMA_BUFFER
      INTEGER*2 I_BUFFER(40000,2)
      EQUIVALENCE (EMA_BUFFER,I_BUFFER)
!
      character*2 LHS_chr,LTC_chr,LTE_chr,LDR_chr,LDE_chr,LDT_chr, &
     &            LZZ_chr
      character*14 lnami_chr
      character*10 lkout1_chr
      character*80 lhbuff_chr
      equivalence (LHS,LHS_chr),(LTC,LTC_chr),(LTE,LTE_chr), &
     &    (LDR,LDR_chr),(LDE,LDE_chr),(LDT,LDT_chr),(LZZ,LZZ_chr)
      equivalence (lhbuff(1),lhbuff_chr)
      equivalence (lnami(1),lnami_chr),(lkout1(1),lkout1_chr)
