      PROGRAM DBHSG
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'catalog_parameters.i'
!
      integer*2 getunit, luo, lui, iveri, ivero, kerr, mtest
      INTEGER*4  ios
      integer*2 nhist,   i,   kc, jmax, nverr,kurc,ierr
      integer*2 iex, ilen, ket, ntoc, knum, ndim, ii
      integer*2 ndd, nde, max0, iread,itr, ileft
      integer*2 nd1, nd2, nd3, nver, ktype, nr, nmax, j
      integer*2 nrecs, n1, n2, n3, kk, ll, mm
      integer*2 mpobst, mpobsn, iii,ipoint,iget
      character*1 ketc
      equivalence (ket,ketc)
!
! DIMENSIONS FOR KAI
      integer*2 LFIO(40),JB(40)
      character*80 jb_chr
      CHARACTER*160 JBC
      character*20 namx_chr
      EQUIVALENCE (JB,JBC)
      INTEGER*4 SETUP_BRK,IERR4
      integer*2 LKYNM(5),LKOUT(5),LFTO(40)
      CHARACTER*10 LKYNM_CHR,LKOUT_CHR
      CHARACTER*80 LFTO_CHR,LFIO_CHR
      EQUIVALENCE (LKYNM(1),LKYNM_CHR),(LKOUT(1),LKOUT_CHR), &
     &  (LFTO(1),LFTO_CHR),(LFIO(1),LFIO_CHR)
      integer*2 IHDT(5)
      integer*2 LT(25),IDCB_un(16),NAMX(10),LCODE(7),LTEXT(16)
      equivalence (namx_chr,namx(1))
      equivalence (jb,jb_chr)
      Character*8 access_code
      Character*32 text_discriptor
      INTEGER*2 BLANK_I2
      PARAMETER   ( BLANK_I2 = 2H   )
!
! DIMENSIONS FOR GET AND PUT (formerly all 200).
! These should match size of buffer in dbase96.i.
! Currently assume 2500 real*8, 5000 integer*4, and 10000 integer*2.
!
      REAL*8 DBD(2500),DBB(2500)
      INTEGER*4 JBD(5000),JBB(5000)
      real*8 RBB(2500),RBD(2500)
      integer*2 IBB(10000),LBB(10000),NDO(3),IBD(10000), &
     & LBD(10000)
      integer*2 LTOC(11,100)
      integer*4 ifbrk
      integer*2 NAM           ,KRUC         ,KAD       , KFIN
      character*2 NAM_c       ,KRUC_c       ,KAD_c     , KFIN_c
      equivalence (NAM,NAM_c),(KRUC,KRUC_c),(KAD,KAD_c),(KFIN,KFIN_c)
      LOGICAL*2 COU_LOCAL, continue
!
      COU_LOCAL = CAT_ON_UNIX
      LUI = 5
      LUO = 6
      IERR4 = SETUP_BRK()
      write(6,'(////,"Program DBHSG - Version 97.12.01")')
      GO TO 100
10    WRITE(LUO,9000)
9000  FORMAT(/," Name of operation ? ",$)
      READ(LUI,9001) NAM
9001  FORMAT(40A2)
      call casefold(NAM_c )
      IF(NAM.EQ.2H::) GO TO 9999
      IF(NAM.EQ.2HPH) GO TO 200
      IF(NAM.EQ.2HGH) GO TO 300
      IF(NAM.EQ.2HAD) GO TO 500
      IF(NAM.EQ.2HOP) GO TO 600
      IF(NAM.EQ.2HWR) GO TO 700
      IF(NAM.EQ.2HMO) GO TO 800
      IF(NAM.EQ.2HGE) GO TO 900
      IF(NAM.EQ.2HPU) GO TO 1000
      IF(NAM.EQ.2HDE) GO TO 1100
      IF(NAM.EQ.2HAS) GO TO 1300
      IF(NAM.EQ.2HDU) GO TO 1400
      IF(NAM.EQ.2HMP) GO TO 1600
      IF(NAM.EQ.2HFI) GO TO 2000
      WRITE(LUO,2654)
 2654 FORMAT("*** ILLEGAL COMMAND ***",/, &
     &" :: to quit",/, &
     &" PH to put a history entry ",/, &
     &" GH to get a history entry ",/, &
     &" AD to add to or delete from a table of contents",/, &
     &" OP to open an empty data record",/, &
     &" WR to write a data record",/, &
     &" MO to move to and open an existing data record",/, &
     &" GE to get data from a data record",/, &
     &" PU to put data into a data record",/, &
     &" DE to delete aa existing data record",/, &
     &" AS to ask about information in the table of contents",/, &
     &" DU to use the 'DBHSG' dump procedure",/, &
     &" MP to move past observations",/, &
     &" FI to call finis",/,)
      GO TO 10
! SEGMENT FOR KAI
!
  100 WRITE(LUO,9100)
9100  FORMAT("Type of action (R,U,C) ? ",$)
      KRUC = 0
      READ(LUI,9101) KRUC
      call casefold(KRUC_c )
      IF(KRUC.EQ.2HR ) KRUC = 1
      IF(KRUC.EQ.2HU ) KRUC = 2
      IF(KRUC.EQ.2HC ) KRUC = 3
      IF(KRUC.EQ.2H::) STOP
9101  FORMAT(40A2)
! SKIP TO NEXT OPERATION
      IF(KRUC.LT.1.OR.KRUC.GT.3) STOP
400   CONTINUE
!
! IF IN THE READ MODE SKIP OVER READING OUTPUT FILE INFORMATION.
!
      IF(KRUC.EQ.3) GO TO 450
! READ INPUT FILE
      WRITE(LUO,9400)
9400  FORMAT(/"Name of input database (case insensitive)",/,"?",$)
      READ(LUI,9101) LKYNM
      IF(LKYNM(1).EQ.2H::) STOP
      call casefold(LKYNM_chr )
  450 CONTINUE
      WRITE(LUO,19944)
19944 FORMAT(/"Input version number - 0 for last",/,"?",$)
      IVERI = 0
      READ(LUI,*) IVERI
      WRITE(LUO,19945) LKYNM,IVERI
19945 FORMAT(" Input name ",5A2," version ",I5)
      IF(KRUC.EQ.1) GO TO 475
!
!     READ OUTPUT FILE NAME AND DISCRIPTOR
!
      WRITE(LUO,19940)
19940 FORMAT(/ &
     &"Output database name.", &
     &" (0 for use input name, case insensitive)",/,"?",$)
      READ (LUI,9101) LKOUT
      IF(LKOUT(1).EQ.2H0 ) LKOUT_CHR(1:4) = 'SAME'
      IF(LKOUT(1).EQ.2H::) STOP
      Call casefold(lkout_chr)
      WRITE(LUO,19941)
19941 FORMAT(/"New file discriptor. (0 for same as input.)",/,"?",$)
      READ(LUI,9101) LFTO
      IF(LFTO(1) .EQ. 2H0 ) LFTO_CHR(1:4) = 'SAME'
      IF(LFTO(1) .EQ. 2H::) STOP
!
  475 CONTINUE
!
! OPEN DATA BASE HANDLER WITH A CALL TO KAI.
      CALL KAI( KRUC, INT2(0), INT2(0), LUI, LKYNM_CHR, IVERI, LKOUT_CHR, &
     &     LFTO_CHR, IVERO, LFIO_CHR, KERR )
! IF IN THE UPDATE OR READ MODE WRITE INFORMATION RETURNED FROM KAI.
      IF(KRUC.EQ.3) GO TO 550
      WRITE(LUO,9404) IVERO,LFIO,KERR
 9404 FORMAT(/" ivero ",I10,/," lfko ",40A2,/," kerr ",I5)
  550 CONTINUE
      IF(KERR.NE.0) WRITE(LUO,9410) KERR
9410  FORMAT("KERR FROM KAI",I3)
      mtest = 1
      if(kerr.eq.0) go to 200
      GO TO 10
! SEGMENT FOR PHIST
!
! READ HISTORY ENTRIES AND PUT INTO FILE
  200 IF(( MTEST.NE.1 .AND. MTEST.NE.2) .OR. KRUC.EQ.1) GO TO 8888
      ios = 1
      do while(ios.ne.0)
        WRITE(LUO,9710)
9710    FORMAT("Number of history entries followed by entries? ",$)
        READ(LUI,*,iostat=ios) NHIST
      enddo
      DO I=1,NHIST
        jb_chr = ' '
        write(luo,'("? ",$)')
        READ(LUI,'(a)') jb_chr
        CALL PHIST( INT2(80), JB_chr )
      enddo
      MTEST = 2
      GO TO 10
!
! SECTION FOR GHIST.
! READ HISTORY CONTROL PARAMETER
  300 IF( MTEST.NE.1) GO TO 8888
      WRITE(LUO,9810)
9810  FORMAT(" GHIST control parameter.  0-next, 1-last. ",$)
      READ(LUI,*) KC
      CALL GHIST( KC, INT2(40), JBc, JMAX, IHDT, NVERR, KERR )
      WRITE(LUO,9020) KERR,IHDT,NVERR,(JB(I),I=1,JMAX)
 9020 FORMAT(" History - Kerr=",I5," Date/Time ",5I5," nverr= ",I5, &
     &/,100(1X,40A2,/))
      GO TO 10
! SEGMENT FOR ADD
!
! READ CONTROL ADD OR DELETE
  500 IF( (MTEST.NE.2 .AND. MTEST.NE.3) .OR. (KURC.EQ.1) ) GO TO 8888
  502 WRITE(LUO,7810)
7810  FORMAT("Add or delete ((A)/D) ? ",$)
      READ(LUI,9101) KAD
      call casefold(KAD_c )
      If(kad_c .ne.'D') kad_c = 'A'
! READ INDEX FILE FOR DATA RECORD TYPE, ELEMENT TYPE, DIMENSIONS, CODE,
! AND TEXT DESCRIPTION FOR ALL ELEMENTS TO BE ADDED
420   WRITE(LUO,5620)
5620  FORMAT("Name of index file - :: for stop ? ",$)
!      READ(LUI,9701) LLNAM
      call blank_buffer( namx_chr, INT2(20) )
      READ(LUI,'(a)') NAMX_chr
9701  FORMAT(3a2)
      IF(NAMX(1).NE.2H::) GO TO 7400
      CALL FINIS( INT2(1) )
      STOP
7400  continue
      idcb_un(1) = getunit()
      OPEN(unit=IDCB_un(1),iostat=IOS,file=NAMX_chr,status='OLD')
      IERR = IOS
      IF(KERR.LT.0) GO TO 420     ! possible ERROR (IERR not KERR) - lf 6/3/88
! LOOP TO READ INDEX FILE
      DO 439 IEX=1,32767
        jb_chr = ' '
        CALL READF_ASC( IDCB_un(1), IERR, JB, INT2(40), ilen )
        IF(ilen.EQ.-1) GO TO 440
        READ (JBC,3425,iostat=ios) (LT(I),I=1,5), &
     &  access_code,text_discriptor
        if(ios.eq.0) then
          WRITE(LUO,3425) (LT(I),I=1,5),access_code,text_discriptor
3425      FORMAT(5I3,1X,A8,1X,A32)
        else
          write(luo,'("ADD: Unable to read lcode card",/, &
     &    "card: ",a)') jbc
          call finis( INT2(1) )
          stop 1
        endif
!
!       ADD OR DELETE ELEMENTS TO DATA BASE FILE
!       FIND ELEMENT TYPE
        KET=LT(2)
        GO TO 7500
 7401   WRITE(LUO,7601)
 7601   FORMAT(" DATA RECORD TYPE?",$)
        READ(LUI,*) LT(1)
        WRITE(LUO,7602)
 7602   FORMAT(" DATA TYPE - R,I,A,J,D ?",$)
        READ(LUI,'(a1)',iostat=ios) KETc
        IF(KETc .EQ.'R') KET = 1
        IF(KETc .EQ.'I') KET = 2
        IF(KETc .EQ.'A') KET = 3
        IF(KETc .EQ.'D') KET = 4
        IF(KETc .EQ.'J') KET = 5
      WRITE(LUO,7604)
      DO 7606 I=6,25
 7606 LT(I) = BLANK_I2
 7604 FORMAT(" LCODE?")
      READ(LUI,7605) (LT(I),I=6,9)
 7605 FORMAT(20A2)
      WRITE(LUO,7610)
 7610 FORMAT(" TEXT DISCRIPTOR (32 CHARACTERS) ?",$)
      READ(LUI,7605) (LT(I), I=10,25)
      WRITE(LUO,7607)
 7607 FORMAT(" DIMENSIONS?",$)
      READ(LUI,*) (LT(I),I=3,5)
 7500 CONTINUE
      GO TO (431,432,433,434,435),KET
431   IF(KAD.EQ. &
     &1HA)CALL ADDR(LT(1),access_code,text_discriptor,LT(3),LT(4),LT(5))
      IF(KAD.EQ.1HD) CALL DELR(LT(1),access_code)
      GO TO 438
432   IF(KAD.EQ. &
     &1HA)CALL ADDI(LT(1),access_code,text_discriptor,LT(3),LT(4),LT(5))
      IF(KAD.EQ.1HD) CALL DELI(LT(1),access_code)
      GO TO 438
433   IF(KAD.EQ.1HD) CALL DELA(LT(1),access_code)
      IF(KAD.EQ. &
     &   1HA)CALL ADDA(LT(1),access_code,text_discriptor,LT(3),LT(4),LT(5))
      GO TO 438
434   IF(KAD.EQ. &
     &1HA)CALL ADDR(LT(1),access_code,text_discriptor,LT(3),LT(4),LT(5))
      IF(KAD.EQ.1HD) CALL DELR(LT(1),access_code)
      GO TO 438
435   IF(KAD.EQ. &
     &1HA)CALL ADDJ(LT(1),access_code,text_discriptor,LT(3),LT(4),LT(5))
      IF(KAD.EQ.1HD) CALL DELJ(LT(1),access_code)
      GO TO 438
438   CONTINUE
      MTEST = 3
      IF(NAMX(1).EQ.2H::) GO TO 10
439   CONTINUE
440   CONTINUE
!      CALL CLOSE(IDCB)
      CLOSE(unit=IDCB_un(1))
      GO TO 10
! SEGMENT FOR OPREC
!
! READ DATA RECORD TYPE TO BE OPENED
  600 IF( (MTEST.NE.3 .AND. MTEST.NE.5) .OR. KRUC.EQ.1 ) GO TO 8888
      WRITE(LUO,9166)
9166  FORMAT("DATA RECORD TYPE TO BE OPENED ? ",$)
      READ(LUI,*) NTOC
      CALL OPREC(NTOC )
      MTEST = 4
      GO TO 10
! SEGMENT FOR WRIDR
  700 IF( MTEST.NE.4 .OR. KRUC.EQ.1) GO TO 8888
      CALL WRIDR()
!
      MTEST = 5
      GO TO 10
! SECTION FOR MVREC
! READ DATA RECORD TYPE TO BE MOVED TO.
  800 IF(KRUC.EQ.1) GO TO 801
      IF( (MTEST.NE.2 .AND. MTEST.NE.3 .AND. MTEST.NE.5) .OR. KRUC.EQ. &
     &   3)GO TO 8888
  801 WRITE(LUO,9888)
9888  FORMAT("Data record type to be moved to ? ",$)
      READ(LUI,*) NTOC
!
      KNUM = 1
      IF(NTOC.EQ.0) GO TO 805
!     FIND HOW MANY RECORDS TO SKIP.
      WRITE(LUO,9777)
 9777 FORMAT( "How many occurances forward should I move? ",$)
      READ(LUI,*) KNUM
  805 CALL MVREC( NTOC, INT2(1), KNUM, KERR)
      WRITE(LUO,9854) KERR
 9854 FORMAT(" KERR from MVREC is ",I10)
      If(kerr.ne.1) MTEST = 4
      GO TO 10
! SECTION FOR GET
! READ PUT FILE FOR DATA RECORD TYPE, ELEMENT TYPE, DIMENSIONS, CODE,
! AND TEXT DESCRIPTION FOR ALL ELEMENTS TO BE PUT. NEXT LINE CONTAINS
! VALUES IN * FORMAT OR 40A2.
  900 IF( MTEST.NE.4 ) GO TO 8888
      WRITE(LUO,9920)
9920  FORMAT("NAME OF GET FILE ?  {0 FOR NO GET FILE}",$)
      call blank_buffer( namx_chr, INT2(20) )
      READ(LUI,'(a)') NAMX_chr
      WRITE(LUO,5111) NAMX
 5111 FORMAT("   ",/,"    ",/," GET FILE NAME - ",3A2,"**********", &
     &"**********************")
      OPEN(unit=IDCB_un(1),iostat=IOS,file=NAMX_chr,status='OLD')
      IERR = IOS
      if(ierr.gt.0) then
        write(luo,5112) ierr,namx
 5112   FORMAT("ERROR ",I5, " OPENING ",3A2)
        go to 900
      endif
! LOOP TO READ GET FILE
      DO 939 IEX=1,32767
      CALL READF_ASC( IDCB_un(1), IERR, JB, INT2(40), ilen )
      IF(ilen.EQ.-1) GO TO 940
      READ (JBC,3425) (LT(i),i=1,5),access_code,text_discriptor
      WRITE(LUO,3425) (LT(i),i=1,5),access_code,text_discriptor
      DO 935 I = 10,12
      LT(I) = 0
 935  CONTINUE
! PUT ELEMENTS INTO DATA BASE FILE
! FIND ELEMENT TYPE
      KET=LT(2)
! CALCULATE TOTAL NUMBER OF VALUES
      NDIM=LT(3)*LT(4)*LT(5)
      GO TO (931,932,933,9331,9332),KET
! CASE FOR REAL VALUES
 931  CALL GETR(access_code,RBD,LT(3),LT(4),LT(5),NDO,KERR)
      IF (KERR.NE.0) THEN !ERROR
        IF (KERR.EQ. &     !NOT FOUND
     &    1)WRITE(LUO, '("REQUESTED LCODE NOT FOUND")')
        IF (KERR.EQ. &     !DIMENSION MISMATCH
     &    2)WRITE(LUO, '("DIMENSION MISMATCH")')
        WRITE(LUO,4931) (LT(II), II=6,9),KERR
 4931 FORMAT(1X,4A2," KERR = ",I4)
      ELSE                !FOUND
        NDD = LT(3)*LT(4)*LT(5)
        NDE = NDO(1)*NDO(2)*NDO(3)
        NDD = MAX0(NDD,NDE)
        WRITE(LUO, '("      FOUND              ")')
        WRITE(LUO,9535)KERR,NDO,(RBD(I),I=1,NDD)
 9535   FORMAT(" FROM THE DATA BASE HANDLER ",/," KERR = ",I5, &
     & " NDO ",3I7,/,100(1X,4D19.11,/))
      END IF
      WRITE(LUO, '("   ")')
      GO TO 938
! CASE FOR INTEGER VALUES
 932  CALL GETI(access_code,IBD,LT(3),LT(4),LT(5),NDO,KERR)
      IF (KERR.NE.0) THEN !ERROR
        IF (KERR.EQ. &     !NOT FOUND
     &    1)WRITE(LUO, '("REQUESTED LCODE NOT FOUND")')
        IF (KERR.EQ. &     !DIMENSION MISMATCH
     &    2)WRITE(LUO, '("DIMENSION MISMATCH")')
        WRITE(LUO,4931) (LT(II), II=6,9),KERR
      ELSE                !FOUND
        NDD = LT(3)*LT(4)*LT(5)
        NDE = NDO(1)*NDO(2)*NDO(3)
        NDD = MAX0(NDD,NDE)
        WRITE(LUO, '("      FOUND              ")')
        WRITE(LUO,9437) KERR,NDO,(IBD(II),II=1,NDD)
 9437   FORMAT(" FROM THE DATA BASE HANDLER ",/," KERR = ",I5, &
     &" NDO = ",3I7,/,100(1X,10I7,/))
      END IF
      WRITE(LUO, '("   ")')
      GO TO 938
! CASE FOR ALPHAMERIC VALUES
  933 CALL GETA(access_code,LBD,LT(3),LT(4),LT(5),NDO,KERR)
      IF (KERR.NE.0) THEN !ERROR
        IF (KERR.EQ. &     !NOT FOUND
     &    1)WRITE(LUO, '("REQUESTED LCODE NOT FOUND")')
        IF (KERR.EQ. &     !DIMENSION MISMATCH
     &    2)WRITE(LUO, '("DIMENSION MISMATCH")')
        WRITE(LUO,4931) (LT(II), II=6,9),KERR
      ELSE                !FOUND
        NDD = LT(3)*LT(4)*LT(5)
        NDE = NDO(1)*NDO(2)*NDO(3)
        NDD = MAX0(NDD,NDE)
        WRITE(LUO, '("      FOUND              ")')
        WRITE(LUO,9525) KERR,NDO,(LBD(II),II=1,NDD)
 9525   FORMAT(" FROM THE DATA BASE HANDLER ",/," KERR = ",I5, &
     & " NDO = ", 3I7,/,100(40A2,/))
      END IF
      WRITE(LUO, '("   ")')
      GO TO 938
!     REAL*8
9331  CALL GETR(access_code,DBD,LT(3),LT(4),LT(5),NDO,KERR)
      IF (KERR.NE.0) THEN !ERROR
        IF (KERR.EQ. &     !NOT FOUND
     &    1)WRITE(LUO, '("REQUESTED LCODE NOT FOUND")')
        IF (KERR.EQ. &     !DIMENSION MISMATCH
     &    2)WRITE(LUO, '("DIMENSION MISMATCH")')
        WRITE(LUO,4931) (LT(II), II=6,9),KERR
      ELSE                !FOUND
        NDD = LT(3)*LT(4)*LT(5)
        NDE = NDO(1)*NDO(2)*NDO(3)
        NDD = MAX0(NDD,NDE)
        WRITE(LUO, '("      FOUND              ")')
        WRITE(LUO,9535) KERR,NDO,(DBD(I),I=1,NDD)
      END IF
      WRITE(LUO, '("   ")')
      GO TO 938
!     INTEGER*4
9332  CALL GETJ(access_code,JBD,LT(3),LT(4),LT(5),NDO,KERR)
      IF (KERR.NE.0) THEN !ERROR
        IF (KERR.EQ. &     !NOT FOUND
     &    1)WRITE(LUO, '("REQUESTED LCODE NOT FOUND")')
        IF (KERR.EQ. &     !DIMENSION MISMATCH
     &    2)WRITE(LUO, '("DIMENSION MISMATCH")')
        WRITE(LUO,4931) (LT(II), II=6,9),KERR
      ELSE                !FOUND
        NDD = LT(3)*LT(4)*LT(5)
        NDE = NDO(1)*NDO(2)*NDO(3)
        NDD = MAX0(NDD,NDE)
        WRITE(LUO, '("      FOUND              ")')
        WRITE(LUO,9437) KERR,NDO,(JBD(I),I=1,NDD)
      END IF
      WRITE(LUO, '("    ")')
938   CONTINUE
939   CONTINUE
940   CONTINUE
!      CALL CLOSE(IDCB)
      CLOSE(unit=IDCB_un(1))
      GO TO 10
!
! SECTION FOR PUT
! READ PUT FILE FOR DATA RECORD TYPE, ELEMENT TYPE, DIMENSTONS, CODE,
! AND TEXT DESCRIPTION FOR ALL ELEMENTS TO BE PUT, NEXT LINE CONTAINS
! VALUES IN * FORMAT OR 40A2.
 1000 IF( MTEST.NE.4 .OR. KRUC.EQ.1) GO TO 8888
      WRITE(LUO,6420)
6420  FORMAT("NAMR OF PUT FILE ? ",$)
      call blank_buffer( namx_chr, INT2(20) )
      READ(LUI,'(a)') NAMX_chr
      OPEN(unit=IDCB_un(1),iostat=IOS,file=NAMX_chr,status='OLD')
      IERR = IOS
      IF(IERR.GT.0) GO TO 1000
! LOOP TO READ PUT FILE
      DO 1039 IEX=1,32767
      CALL READF_ASC( IDCB_un(1), IERR, JB, INT2(40), ilen )
      IF(ilen.EQ.-1) GO TO 1040
      READ (JBC,3425) (LT(i),i=1,5),access_code
      WRITE(LUO,3425) (LT(i),i=1,5),access_code
      DO 1035 I = 10,12
      LT(I) = 0
 1035 CONTINUE
      DO 1036 I= 1,7
            LCODE(I) = 0
 1036 CONTINUE
      DO 1037 I= 1,4
      LCODE(I) = LT(5+I)
 1037 CONTINUE
! PUT ELEMENTS INTO DATA BASE FILE
! FIND ELEMENT TYPE
      KET=LT(2)
! CALCULATE TOTAL NUMBER OF VALUES
      NDIM=LT(3)*LT(4)*LT(5)
      GO TO (1031,1032,1033,21031,21032),KET
!
! CASE FOR REAL VALUES
 1031 IREAD = 0
10000 CONTINUE
        ITR = 3
        ILEFT = NDIM - IREAD
        IF(ILEFT.EQ.0) GO TO 11000
        IF(ILEFT.LT.ITR) ITR =ILEFT
!       READ DATA CARD
        jbc = ' '
        CALL READF_ASC( IDCB_un(1), IERR, JB, INT2(40), ilen )
        WRITE(LUO,10002) JB
10002   FORMAT(40A2)
        READ(JBC,*,iostat=ios) (RBB(I),I=IREAD+1,IREAD+ITR)
        if(ios.ne.0) then
          write(luo,'("put/reals: error read data card.")')
          write(luo,'("card:",a80)') jbc
          call finis( INT2(1) )
          stop
        endif
        IREAD = IREAD + ITR
      GO TO 10000
11000 CONTINUE
      WRITE(LUO,8431) (RBB(I),I=1,NDIM)
8431  FORMAT(4D19.11)
      CALL PUTR(access_code,RBB,LT(3),LT(4),LT(5) )
      WRITE(LUO,5689) LCODE
 5689 FORMAT("PUTR ",4A2,3I10)
      GO TO 1038
!
! CASE FOR INTEGER VALUES
1032  ileft = ndim
      ipoint = 1
      continue = .true.
      do while(continue)
        if(ileft .gt. 10) then
          iget = 10
          ileft = ileft-10
        else
          iget = ileft
          continue = .false.
        endif
!       read data card
        jbc = ' '
        CALL READF_ASC( IDCB_un(1), IERR, JB, INT2(40), ilen )
        READ(JBC,*,iostat=ios) (IBB(I),I=ipoint,ipoint+iget-1)
        if(ios.ne.0) then
          write(luo,'("put/i*2  : error read data card.")')
          write(luo,'("card:",a80)') jbc
          call finis( INT2(1) )
          stop
        endif
        ipoint = ipoint+ileft
      enddo
      WRITE(LUO,'(10i7)') (IBB(I),I=1,NDIM)
      CALL PUTI(access_code,IBB,LT(3),LT(4),LT(5) )
      WRITE(LUO,5690) LCODE
5690  FORMAT("PUTI ",4A2,3I10)
      GO TO 1038
!
! CASE FOR ALPHAMERIC VALUES
1033  ileft = ndim
      ipoint = 1
      continue = .true.
      do while(continue)
        if(ileft .gt. 40) then
          iget = 40
          ileft = ileft-40
        else
          iget = ileft
          continue = .false.
        endif
!       read data card
        jbc = ' '
        CALL READF_ASC( IDCB_un(1), IERR, JB, INT2(40), ilen )
        READ(JBC,'(40a2)',iostat=ios)(LBB(I),I=ipoint,ipoint+iget-1)
        if(ios.ne.0) then
          write(luo,'("put/alpha: error read data card.")')
          write(luo,'("card:",a80)') jbc
          call finis( INT2(1) )
          stop
        endif
        ipoint = ipoint+ileft
      enddo
      WRITE(LUO,9101) (LBB(I),I=1,NDIM)
      CALL PUTA(access_code,LBB,LT(3),LT(4),LT(5) )
      WRITE(LUO,5691) LCODE
5691  FORMAT("PUTA ",4A2,3I10)
      GO TO 1038
!
!     REAL*8
21031 IREAD = 0
20000 CONTINUE
      ITR = 3
      ILEFT = NDIM - IREAD
      IF(ILEFT.EQ.0) GO TO 21000
      IF(ILEFT.LT.ITR) ITR =ILEFT
!     READ DATA CARD
      jbc = ' '
      CALL READF_ASC( IDCB_un(1), IERR, JB, INT2(40), ilen )
      WRITE(LUO,20002) JB
20002 FORMAT(40A2)
      READ(JBC,*,iostat=ios)(DBB(I),I=IREAD+1,IREAD+ITR)
      if(ios.ne.0) then
        write(luo,'("put/real/d: error read data card.")')
        write(luo,'("card:",a80)') jbc
        call finis( INT2(1) )
        stop
      endif
      IREAD = IREAD + ITR
      GO TO 20000
21000 CONTINUE
      WRITE(LUO,8431) (DBB(I),I=1,NDIM)
      CALL PUTR(access_code,DBB,LT(3),LT(4),LT(5) )
      WRITE(LUO,25689) LCODE
25689 FORMAT("PUTR ",4A2,3I10)
      GO TO 1038
!
! CASE FOR INTEGER:4 VALUES
!     READ DATA CARD
!
21032 ileft = ndim
      ipoint = 1
      continue = .true.
      do while(continue)
        if(ileft .gt. 10) then
          iget = 10
          ileft = ileft-10
        else
          iget = ileft
          continue = .false.
        endif
!       read data card
        jbc = ' '
        CALL READF_ASC( IDCB_un(1), IERR, JB, INT2(40), ilen )
        READ(JBC,*,iostat=ios) (jbb(I),I=ipoint,ipoint+iget-1)
        if(ios.ne.0) then
          write(luo,'("put/i*4: error read data card.")')
          write(luo,'("card:",a80)') jbc
          call finis( INT2(1) )
          stop
        endif
        ipoint = ipoint+ileft
      enddo
      READ(JBC,*) (JBB(I),I=1,NDIM)
      WRITE(LUO,'(10i7)') (JBB(I),I=1,NDIM)
      CALL PUTJ(access_code,JBB,LT(3),LT(4),LT(5) )
      WRITE(LUO,25690) LCODE
25690 FORMAT("PUTJ ",4A2,3I10)
      GO TO 1038
1038  CONTINUE
1039  CONTINUE
1040  CONTINUE
!      CALL CLOSE(IDCB)
      CLOSE(unit=IDCB_un(1))
      GO TO 10
! SECTION FOR DELDR
 1100 IF( MTEST.NE.4 .OR. KRUC.NE.2) GO TO 8888
      CALL DELDR()
      MTEST = 5
      GO TO 10
! SECTION FOR MOVE PAST
!
 1200 WRITE(LUO,1222)
 1222 FORMAT(" Move past no longer functional")
      GO TO 10
! SECTION FOR ASK
!
 1300 IF( MTEST.LT.1 .OR. MTEST.GT.3 ) GO TO 8888
      WRITE(LUO,9301)
 9301 FORMAT(" WHAT IS THE NTOC AND LCODE?")
      READ(LUI,9310)NTOC,(LCODE(I),I=1,4)
 9310 FORMAT(I1,4A2)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      CALL ASK(LCODE,NTOC,ND1,ND2,ND3,NVER,LTEXT,KTYPE,KERR )
      IF(KERR.EQ.0) then
        go to 1320
      else
      go to 1330
      endif
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 1320 WRITE(LUO,9302) (LCODE(I),I=1,4),NTOC,ND1,ND2,ND3,NVER,KTYPE, &
     & LTEXT
 9302 FORMAT(" INPUT LCODE = ",4A2," NTOC = ",I2,/," DIMENSIONS = ", &
     & 3I7," NVER = ",I7," KTYPE = ",I7,/,1X,16A2)
      MTEST = 3
      GO TO 10
 1330 WRITE(LUO,9393) KERR,NTOC,(LCODE(I),I=1,4)
 9393 FORMAT(" KERR FROM 'ASK' IS ",2I5,4A2)
      MTEST = 3
      GO TO 10
! SECTION FOR DUMP (RECORDS)
!
 1400 IF( (MTEST.NE.1 .AND. MTEST.NE.2) .OR. KRUC.NE.1) GO TO 8888
      NR = 0
      WRITE(LUO,21400)
21400 FORMAT(" WHAT NTOC DO YOU WANT DUMP?",$)
      READ(LUI,*) NTOC
 1401 WRITE(LUO,21401)
21401 FORMAT(" WHAT IS THE NAMR OF THE DUMP FILE?  ",$)
      call blank_buffer( namx_chr, INT2(20) )
      READ(LUI,'(a)') NAMX_chr
      IF(NAMX(1).EQ.2H::) GO TO 10
      WRITE(LUO,21420)
21420 FORMAT(" HOW MANY RECORDS DO YOU WANT DUMPED?  ",$)
      READ(LUI,*) NMAX
      OPEN(unit=IDCB_un(1),iostat=IOS,file=NAMX_chr,status='OLD')
      IERR = IOS
      IF(IERR.GT.0) GO TO 1401
 1402 CALL READF_ASC( IDCB_un(1), IERR, JB, INT2(40), ilen)
      IF(ilen.LE.0) GO TO 1420
      NR=NR+1
      IF(NR.EQ.100) STOP 3344
      DO 1410 I=1,4
           LTOC(I,NR) = JB(I)
 1410 CONTINUE
      CALL ASK(JB,NTOC,LTOC(8,NR),LTOC(9,NR),LTOC(10,NR),NVER,LTEXT, &
     &     LTOC(11,NR),KERR )
      IF(KERR.EQ.0) GO TO 1411
      WRITE(LUO,21404) (JB(I),I=1,4),KERR
21404 FORMAT(1X,4A2," NOT FOUND BY ASK. KERR = ",I5)
      NR = NR-1
      GO TO 1402
 1411 WRITE(LUO,21405) (LTOC(I,NR),I=1,4),(LTOC(J,NR),J=8,11)
21405 FORMAT(1X,4A2,1X,4I5)
      GO TO 1402
!1420 CALL CLOSE(IDCB)
 1420 CLOSE(unit=IDCB_un(1))
      NRECS = 0
      IF(NR.EQ.0) GO TO 10
 1421 continue
      if(ifbrk().eq.-1) go to 10
 1422 IF(NRECS.EQ.NMAX) GO TO 10
      CALL MVREC( NTOC, INT2(1), INT2(1), KERR )
      IF(KERR.NE.0) GO TO 1450
      NRECS = NRECS+1
      WRITE(LUO,21407) NRECS
21407 FORMAT(//," RECORD NUMBER ",I5)
      DO 1445 II=1,NR
      N1 = LTOC(8,II)
      N2 = LTOC(9,II)
      N3 = LTOC(10,II)
      GO TO (1431,1432,1433),LTOC(11,II)
!
 1431 CALL GETR(LTOC(1,II),RBD,N1,N2,N3,NDO,KERR)
      WRITE(LUO,21406) (LTOC(I,II),I=1,4),(LTOC(J,II),J=8,10)
21406 FORMAT(1X,"**** ",4A2,3I5," **** ")
      DO 1436 KK =1,N3
            DO 1436 LL =1,N2
            WRITE(LUO,21409) (RBD(N1*N2*(KK-1)+N1*(LL-1)+MM),MM=1,N1)
21409 FORMAT(4(1X,D19.11))
 1436 CONTINUE
      GO TO 1445
 1432 CALL GETI(LTOC(1,II),IBD,N1,N2,N3,NDO,KERR)
      WRITE(LUO,21406)(LTOC(I,II),I=1,4),(LTOC(J,II),J=8,10)
      DO 1437 KK=1,N3
           DO 1437 LL =1,N2
           WRITE(LUO,21410) (IBD(N1*N2*(KK-1)+N1*(LL-1)+MM),MM=1,N1)
21410 FORMAT(10I7)
 1437 CONTINUE
      GO TO 1445
 1433 CALL GETA(LTOC(1,II),IBD,N1,N2,N3,NDO,KERR)
      WRITE(LUO,21406) (LTOC(I,II),I=1,4),(LTOC(J,II),J=8,10)
      DO 1438 KK =1,N3
            DO 1438 LL =1,N2
            WRITE(LUO,21411) (IBD(N1*N2*(KK-1)+N1*(LL-1)+MM),MM=1,N1)
21411 FORMAT(1X,40A2)
 1438 CONTINUE
 1445 CONTINUE
      GO TO 1421
 1450 CALL FINIS( INT2(1) )
      STOP
!
!     SECTION FOR MOVE PAST
!
1600  CONTINUE
      WRITE(LUO,1601)
1601  FORMAT(" WHICH TYPE OBS SHOULD I MOVE PAST? ",$)
      READ(LUI,*) MPOBST
      WRITE(LUO,1602)
1602  FORMAT(" HOW MANY SHOULD I MOVE PAST? ",$)
      READ(LUI,*) MPOBSN
      DO 1603 III=1,MPOBSN
      CALL MVREC( MPOBST, INT2(1), INT2(1), KERR )
      IF(KERR .NE. 0) WRITE(LUO,1604) KERR
1604  FORMAT(" KERR FROM MVREC IS ",I6)
      IF(KERR .NE. 0) GO TO 10
      CALL DELDR()
1603  CONTINUE
      WRITE(LUO,1605)
1605  FORMAT(" MP COMPLETED.  NO DATA RECORD IS NOW OPEN.")
      GO TO 10
!
! SECTION FOR FINIS
2000  CONTINUE
      IF( KRUC.NE.1 .AND. MTEST.EQ.4) GO TO 8888
      WRITE(LUO,8577)
 8577 FORMAT("Input to FINIS (n-normal,a-abort,t-terminate here)", &
     &     /,"?",$)
      KFIN = 1
      READ(LUI,9101) KFIN
      call casefold(KFIN_c )
      IF(KFIN.EQ.2HN ) KFIN = 0
      IF(KFIN.EQ.2HA ) KFIN = 1
      IF(KFIN.EQ.2HT ) KFIN = 2
      CALL FINIS(KFIN )
!%      CALL IOF(6HDBCAT ,IERR,-1)
! EXIT
      STOP
 9999 CONTINUE
!
!     SECTION FOR WRITING ILLEGAL COMMAND MESSAGE
!
 8888 WRITE(LUO,8889)
 8889 FORMAT(" ILLEGAL COMMAND AT THIS TIME - WILL CAUSE", &
     &" THE DATA BASE HANDLER TO ERROR STOP")
      GO TO 10
      END
