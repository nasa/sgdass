      PROGRAM DBHD3
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'catalog_parameters.i'
!I2   include '../includes/cattail.ftni'
!
!       PROGRAM TO DUMP MORE THEN ONE TYPE OF DATA BASE RECORD IN A SINGLE
!       RUN
!
!       BRUCE SCHUPLER  -  2 OCTOBER 1979
!       REVISED 18 DECEMBER 1979 TO INCREASE LENGTH OF GHIST'S BUFFER
!
!       05/08/90 BAA Scratch file name defined properly.  READ of ITOCD fixed.
!                    GGC had previously changed length of history entry
!                    buffers to the correct value of 160.
!       03/13/91 BAA Reinstalled above corrections, removed unused errus
!                    routine from end, changed r2dump and sdump counters
!                    to integers.
!       07/23/91 BAA Reinstalled above corrections into GSFC version, and
!                    length of DFILNM increased to reasonable size.
!       07/21/92 JRR Fixed initial assignment of ILU_STDIN and ILU_STDOUT;
!                    fixed dimension of NDIM(*) in subroutines PRINR,
!                    PRINI, PRIND, PRINJ, PRINA.  Also changed ITOC(2,1)
!                    to ITOC(2,*) in DTOC, DREC, and WHICH, and LQ(2,1)
!                    to LQ(2,*) in DBHBS, and IBUF(1) to IBUF(*) in
!                    BLANK.  Changed declaration for RBUF in PRINR to
!                    REAL*8 RBUF(*) and for DBUF in PRIND to DBUF(*).
!                    Also LBUF in prina, prini, and JBUF in prinj.
!       93.08.30 BAA Remerged i*2 catalog version with current i*4 GSFC
!                    version (to create i*2 version).
!       93.11.10 BA  Updated to i*4 catalog use.
!       95.09.18 BA  Fixed compiler warnings (unused and twice defined
!                    variables dropped).
!       95.10.19 BA  "drec" changed so that main lcode buffer (lbuf,
!                    etc.) now comes from dbase96.i include.
!                    Also fixed total number of records request
!                    so it works.
!       96.02.15 JWR Fix default format to original, not dbhsg compatible.
!       96.03.01 jwr "drec' changed so the buffers are local, but sizes
!                    come from dbase96.i.  Undoes chaned made at 95.10.19
!                    which caused dbase's internal buffer to be overwritten.
!       97.11.17 kdb Relink to pick up user tracking corrections.
!       97.12.01 kdb Relink to pick up mnt_names.f correction.
!     2000.06.26 pet Fixed a bug: the old version incorrectly printed values
!                    of lcodes of INTEGER*4  type
!     2003.12.29 kdb Add machine conditional compilation to handle the file
!                    open of the database as binary (LINUX) or unformatted
!                    (HPUX).
!     2005.04.13 kdb Testing of the Linux version on HP-UX revealed that
!                    argument 5 of some READZ calls was being incorrectly 
!                    passed to READZ as 0.  The problem was that some of these
!                    arguments' numeric constants were being passed through
!                    int, not int2.
!     2005.04.19 kdb Testing of the Linux version on HP-UX revealed that
!                    a readf call was not fully converted to a readz call.
!         
!
      COMMON /DBDMP/ IDCBLC_un(16),LNAME_CHR,ISCRCR
      CHARACTER*12 LNAME_CHR
      integer*2 getunit,idcblc_un,iscrcr,maxtoc,ilu_stdin
      INTEGER*4 IOS
      integer*2 ilu_stdout, iveri,ierr,iluin,kerr,jmax,nver
      integer*2 nver_old, i, j, irec, iluout, ilast,numtoc
      integer*2 idump, iquit
      INTEGER*4 IERR4,SETUP_BRK,r2dump,sdump
      INTEGER*2 SCRNLNS
      integer*2 IMPAR(5),         LFIO(40),LHIST(80),IHDT(5)
      integer*2 ITOC(2,100),ITOCD(4,100),ISIZE(2),IDUM(10)
      CHARACTER*20 IDUMC
      character*10 lkynm_chr,same_chr
      character*160 lhist_chr
      character*80 lfio_chr
      character*1 lresp
      Character*256 DFILNM
      CHARACTER*1 CONT
      INTEGER*2 TRIMLEN,DFILEN,ISWTCH
      LOGICAL*2 COU_LOCAL
      LOGICAL*2 NEEDFILE
      logical*2 dbhsg_format
      equivalence (lhist(1),lhist_chr)
      equivalence (lfio_chr,lfio(1))
      EQUIVALENCE (IDUM, IDUMC)
      data dbhsg_format /.false./
      data same_chr/'SAME'/
      data lname_chr /'/tmp/DBSC99'/
      DATA MAXTOC /100/
      DATA ISIZE /300,40/
      CHARACTER  RELEASE_DATE*54, GET_VERSION*54
!
! 1.    FIND OUT WHERE WE ARE AND GREET THE USER
!
      INCLUDE     'dbhd3_version.i'
      COU_LOCAL = CAT_ON_UNIX
      CALL RMPAR(IMPAR )
      ILU_stdIN = 5
      ILU_stdout = 6
      If(impar(1).eq. -99) dbhsg_format = .true.
!
      ierr4 = setup_brk()
      RELEASE_DATE = GET_VERSION ()
!     Initialize scratch file info
      ISCRCR = 16
      WRITE(ILU_stdout,'(///, &
     &"DBHD3 Version ",a," Data base dump program",//, &
     &"Enter experiment name of database to be dumped", &
     &" (case insensitive)",//,"? ",$)') release_date(1:trimlen(release_date))
      read(ilu_stdin,110) LKYNM_chr
110   FORMAT(a10)
      call casefold(LKYNM_chr )
      IF(LKYNM_chr(1:1) .EQ. ':') GO TO 710
      ios = -1
      Do while(ios.ne.0)
        write(ilu_stdout,'(/, &
     &  "Enter version number to be dumped - ",$)')
        read(ilu_stdin,*,iostat=ios) IVERI
      Enddo
!     FIND OUT IF USER WANTS TO DUMP TO FILE OR SCREEN
      NEEDFILE = .TRUE.
      DO WHILE (NEEDFILE)
 798    WRITE (ilu_stdout,799)
 799    FORMAT (/,/, &
     &  "Do you want to dump to a (F)ile "'or to the (S)creen (F/S)? ',$)
        READ(ilu_stdin,320)LRESP
        CALL CASEFOLD(lresp )
        IERR=0
        IF (LRESP.EQ.'S') THEN
        ILUOUT = 6
        NEEDFILE = .FALSE.
        ELSE IF (LRESP.EQ.'F') THEN
 800                  WRITE (ilu_stdout,801)
 801      FORMAT(/,/,"Enter file name into which you want to dump - ",$)
          READ(ilu_stdin,802) DFILNM
 802      FORMAT(A256)
        DFILEN=TRIMLEN(DFILNM)
          CALL SWITCH_AREA(DFILNM,DFILEN,ISWTCH )
          IF (ISWTCH.EQ.1) THEN
          WRITE(6,'("You are not allowed to dump to a file ", &
     &       "on the juke.",/,"Please select a different dump file.")')
          GO TO 800
          ENDIF
        ILUOUT=getunit()
        OPEN(UNIT=ILUOUT,FILE=DFILNM,STATUS='NEW',ERR=803)
        NEEDFILE = .FALSE.
        GO TO 805
 803      WRITE (ilu_stdout,804) DFILNM
 804      FORMAT (/,/,"The file (",A63,") already exists.",/, &
     &      "Do you want to write over it (Y/N)?",$)
        READ (ilu_stdin,320)LRESP
        CALL CASEFOLD(lresp )
        IF (LRESP.EQ.'Y') THEN
          OPEN(UNIT=ILUOUT,FILE=DFILNM,STATUS='OLD',IOSTAT=IOS)
          IERR =IOS
          NEEDFILE = .FALSE.
            IF (IERR.NE.0) THEN
 806          WRITE(ilu_stdout,807)
 807          FORMAT(/,/,"The file, ",A12,", could not be read.")
            NEEDFILE = .TRUE.
            ENDIF
          ELSE
          NEEDFILE = .TRUE.
          ENDIF
      ELSE
          NEEDFILE=.TRUE.
        ENDIF
 805  END DO
!
! 2.    OPEN THE DATA BASE
!
      CALL KAI( INT2(1), INT2(0), INT2(0), ILUIN, LKYNM_chr, IVERI, same_chr, &
     &     same_chr, INT2(0), LFIO_chr, KERR )
      If(kerr.ne.0) then
        Write(ilu_stdout,'(//, &
     &  "Unable to open database ",a10," Version",i4, &
     &  ". Kerr from KAI is", &
     &  i5,//,"Quitting!")') lkynm_chr,iveri,kerr
        If (iluout.ne.ilu_stdout) Write(iluout,'(//, &
     &    "Unable to open database ",a10," Version",i4, &
     &    ". Kerr from KAI is", &
     &    i5,//,"Quitting!")') lkynm_chr,iveri,kerr
        stop
      Endif
!
      IF (ILUOUT.NE.6) WRITE(ILUOUT,200) LFIO_chr
      WRITE(ilu_stdout,200)LFIO_chr
200   FORMAT(//,1X,"The file descriptor is:",/,1X,a80,//)
!
! 3.    SEE IF THE USER WANTS TO SEE THE HISTORY ENTRIES
!
300   CONTINUE
      write(ilu_stdout,310)
310   FORMAT("Do you want to see the history entries (Y/N)? ",$)
      read(ilu_stdin,320) LRESP
      Call casefold(lresp)
320   FORMAT(A1)
      IF(LRESP .EQ. 'N') then
        CALL GHIST( INT2(1), INT2(160), LHIST_chr, JMAX, IHDT, NVER, KERR )
        GO TO 400
      endif
!
!     Dump the history entries
!
325   continue
!     Initialize continue choice
      CONT='Y'
      nver_old = 0
326   call ghist( INT2(0), INT2(160), LHIST_chr, jmax, ihdt, nver, kerr)
      If(kerr .eq. 1) go to 400
      If(nver_old .ne. &
     &nver)then
        nver_old = nver
        write(iluout,'(/, &
     &  " History for version.",I5,":Created :", &
     &  i4,":",i3":",3(i2,":"))') nver,ihdt
        SCRNLNS=SCRNLNS+1
      endif
      write(iluout,'(a75)') lhist_chr(1:75)
      SCRNLNS=SCRNLNS+1
!     Test for number of lines on screen.
      IF ((ILUOUT.EQ.6).AND.(SCRNLNS.GT.19).AND.(CONT.NE.'S')) THEN
        WRITE (ILUOUT,8000)
 8000   FORMAT ("&dBContinue listing ((Y)/N/&@", &
     &       '&dBS to stop asking)? &@',$)
        READ (ilu_stdin,'(A1)') CONT
        SCRNLNS=0
        CALL CASEFOLD(CONT )
        IF (CONT.EQ.'N') GO TO 400
      ENDIF
      GO TO 326
!
! 4.    SETUP THE LCODE FILE
!
400   CONTINUE
!      CALL CREAT(IDCBLC,IERR,LNAME,ISIZE,2,0,ISCRCR)
!      IF((IERR .LT. 0) .AND. (IERR .NE. -2)) write(ilu_stdout,410) IERR
!410   FORMAT("WHILE ATTEMPTING TO CREATE THE DUMP FILE IERR WAS ",I5)
!      IF((IERR .LT. 0) .AND. (IERR .NE. -2)) GO TO 700
!      CALL OPEN(IDCBLC,IERR,LNAME,2B,0,ISCRCR)
!      IF(IERR .LT. 0) write(ilu_stdout,420) IERR
!420   FORMAT("WHILE ATTEMPTING TO OPEN THE DUMP FILE IERR WAS ",I5)
!      IF(IERR .LT. 0) GO TO 700
!
      idcblc_un(1) = getunit()
      open(unit=idcblc_un(1),file=lname_chr,status='UNKNOWN',recl=80, &
     &     access='DIRECT',form='UNFORMATTED',iostat=ios)
      IERR = IOS
      IF(IERR .GT. 0) then
        write(ilu_stdout,410) IERR
410     FORMAT("OPEN ERROR ",I5," WAS RECEIVED ON NEW DUMP FILE.")
        go to 700
      endif
!
      DO 430 I=1,MAXTOC
      ITOC(1,I) = 0
      ITOC(2,I) = 0
      ITOCD(1,I) = 0
430   CONTINUE
      call finis( INT2(1) )
      CALL DBHBS(LKYNM_chr,IVERI,ILU_stdout,IDCBLC_un(1),ITOC,MAXTOC, &
     &       ILAST,KERR )
      IF(KERR .NE. 0) GO TO 700
!
! 5.    SEE WHAT TOC'S WE HAVE
!
      DO 500 I=1,MAXTOC
      NUMTOC = I
      IF(ITOC(1,I) .EQ. 0) GO TO 510
500   CONTINUE
510   CONTINUE
      NUMTOC = NUMTOC - 1
!
! 5.1   SEE IF THE USER IS INTERESTED IN LOOKING AT THE TOC ENTRIES
!
      write(ilu_stdout,520)
520   FORMAT("Do you want to see the TOC entries (Y/N)? ",$)
      read(ilu_stdin,320) LRESP
      call casefold(lresp )
      IF(LRESP .EQ. 'N') GO TO 600
      write(ilu_stdout,530) LKYNM_chr,NVER,(ITOC(1,J),J=1,NUMTOC)
530   FORMAT("Data base ",a10," Version ",I3," has records of type ",/, &
     &       15(I3,1X),/,15(I3,1X),/,15(I3,1X),/,15(I3,1X),/,15(I3,1X), &
     &     /,15(I3,1X),/,15(I3,1X),/,15(I3,1X),/,15(I3,1X),/,15(I3,1X), &
     &     /,15(I3,1X),/,15(I3,1X),/,15(I3,1X),/,5(I3,1X))
      write(ilu_stdout,531)
531   FORMAT("Which record types do you want to see the TOC for ", &
     &     "(0 for all, :: to end list)")
      IDUMP = 1
540   CONTINUE
      write(ilu_stdout,541)
541   FORMAT("? ",$)
      read(ilu_stdin,'(a2)') IDUMC(1:2)
      IF(IDUMC(1:2).eq.'::') GO TO 1541
      READ(IDUMC,*,iostat=ios) ITOCD(1,IDUMP)
      If(ios.ne.0) then
        Write(ilu_stdout,'( &
     &  "Could not read that record type. Try again.")')
        Go to 540
      Endif
      GO TO 1542
 1541 ITOCD(1,IDUMP) = -1
 1542 CONTINUE
      IF((ITOCD(1,IDUMP) .EQ. 0) .OR. (ITOCD(1,IDUMP) .EQ. -1) .OR. &
     &   (IDUMP .GE. MAXTOC)) GO TO 545
      IDUMP = IDUMP + 1
      GO TO 540
!
! 5.2   DUMP THE REQUESTED TOC'S
!
545   CONTINUE
      IDUMP = IDUMP - 1
!
! 5.2.1 DO WE DUMP THEM ALL?
!
      IF(ITOCD(1,1) .NE. 0) GO TO 560
!
! 5.2.2 YES
!
      DO 550 I=1,NUMTOC
      CALL DTOC(ILUOUT,ITOC,I,ILAST,MAXTOC,LKYNM_chr, &
     &     NVER,IDCBLC_un(1),dbhsg_format )
550   CONTINUE
      GO TO 600
!
! 5.2.3 NO
!
560   CONTINUE
      DO 580 I=1,NUMTOC
        DO 570 J=1,IDUMP
        IF(ITOCD(1,J) .NE. ITOC(1,I)) GO TO 570
        CALL DTOC(ILUOUT,ITOC,I,ILAST,MAXTOC,LKYNM_chr,NVER, &
     &              IDCBLC_un(1),dbhsg_format )
        GO TO 580
570     CONTINUE
580   CONTINUE
      GO TO 600
!
! 6.    NOW FOR THE DUMP ITSELF
!
600   CONTINUE
      CALL KAI( INT2(1), INT2(0), INT2(0), ILUIN, LKYNM_chr, IVERI, same_chr, &
     &     same_chr, INT2(0), LFIO_chr, KERR )
      DO 610 I=1,MAXTOC
      ITOCD(1,I) = 0
      ITOCD(2,I) = 0
      ITOCD(3,I) = 0
      ITOCD(4,I) = 0
610   CONTINUE
!
! 6.1   SEE WHAT TOC'S THE USER WANTS TO DUMP
!
      write(ilu_stdout,620) LKYNM_chr,NVER,(ITOC(1,J),J=1,NUMTOC)
620   FORMAT(//,"Data base ",a10," Version ",I3," has records of type ", &
     &       /,15(I3,1X),/,15(I3,1X),/,15(I3,1X),/,15(I3,1X),/,15(I3,1X), &
     &     /,15(I3,1X),/,15(I3,1X),/,15(I3,1X),/,15(I3,1X),/,15(I3,1X), &
     &     /,15(I3,1X),/,15(I3,1X),/,15(I3,1X),/,5(I3,1X))
      write(ilu_stdout,621)
621   FORMAT("Which record types do you want to see the dump for ", &
     &     "(0 for all, :: to end list)")
      IDUMP = 1
      CONT='Y'
630   CONTINUE
      ios = -1
      Do while (ios.ne.0)
        write(ilu_stdout,541)
        read(ilu_stdin,'(A2)') IDUMc(1:2)
        IF(IDUMc(1:2) .EQ.'::') GO TO 1630
        READ(IDUMC,*,iostat=ios) ITOCD(1,IDUMP)
        If(ios.ne.0) write(ilu_stdout,'("Invalid! Try again.")')
      Enddo
      GO TO 1631
 1630 ITOCD(1,IDUMP) = -1
 1631 CONTINUE
      IF((ITOCD(1,IDUMP) .EQ. 0) .OR. (ITOCD(1,IDUMP) .EQ. -1) .OR. &
     &   (IDUMP .GE. MAXTOC)) GO TO 640
      IDUMP = IDUMP + 1
      GO TO 630
640   CONTINUE
      IF(ITOCD(1,1) .EQ. -1) GO TO 700
      IDUMP = IDUMP - 1
!
! 6.2   ARE WE DUMPING ALL OF THE TYPES OF RECORDS?
!
      IF(ITOCD(1,1) .NE. 0) GO TO 670
      write(ilu_stdout,650)
650   &
     & FORMAT("How many total records (all types included) do you wish"," to see (0 for all)? ",$)
      read(ilu_stdin,*) R2DUMP
      IF(R2DUMP .LT. 0) GO TO 700
!
! 6.3   YES.  LET'S GO LOOKING
!
      SDUMP = 0
!^^^^
660   CONTINUE
      SDUMP = SDUMP + 1
      IF((SDUMP .GT. R2DUMP) .AND. (R2DUMP .ne. 0)) GO TO 700
!^^^^
      CALL MVREC( INT2(0), INT2(1), INT2(1), IERR )
      IF(IERR .GT. 0) GO TO 700
      IREC = IABS(IERR)
      DO 662 I=1,NUMTOC
      IF(ITOC(1,I) .NE. IREC) GO TO 662
      ITOCD(4,I) = ITOCD(4,I) + 1
      IF (CONT.EQ.'N') GO TO 700
      CALL DREC(ILUOUT,ITOC,IREC,ITOCD(4,I),LKYNM_chr,NVER,ILAST,MAXTOC, &
     &          IDCBLC_un(1),CONT,dbhsg_format )
      GO TO 663
662   CONTINUE
663   CONTINUE
      GO TO 660
!
! 6.4   NO.  JUST SOME TYPES
!
670   CONTINUE
      DO 680 I=1,IDUMP
      write(ilu_stdout,685) ITOCD(1,I)
685   FORMAT("Enter the first and last record of type ",I3," that you ", &
     &       "want to see (0,0 for all)",/,"? ",$)
      read(ilu_stdin,*) ITOCD(2,I),ITOCD(3,I)
      IF(ITOCD(2,I) .EQ. 0) ITOCD(3,I) = 0
      CALL WHICH(ITOCD(1,I),ITOC,ILU_stdIN,ilu_stdout,IDCBLC_un(1), &
     &           MAXTOC,ILAST )
680   CONTINUE
!
! 6.4.1 DO THE SPECIFIC DUMP
!
682   CONTINUE
      IF(IDUMP .NE. 1)CALL MVREC( INT2(0), INT2(1), INT2(1), IERR)
      IF(IDUMP .EQ. 1)CALL MVREC( ITOCD(1,1), INT2(1), INT2(1), IERR)
      IF(IERR .GT. 0) GO TO 700
      IREC = IABS(IERR)
      IF(IDUMP .EQ. 1) IREC = ITOCD(1,1)
      DO 690 I=1,IDUMP
      IF(IREC .NE. ITOCD(1,I)) GO TO 690
      ITOCD(4,I) = ITOCD(4,I) + 1
      IF((ITOCD(3,I) .LT. ITOCD(4,I)) .AND. (ITOCD(3,I) .NE. &
     &    0))GO TO 690
      IF(ITOCD(4,I) .LT. ITOCD(2,I)) GO TO 690
      IF(CONT.EQ.'N') GO TO 700
      CALL DREC(ILUOUT,ITOC,IREC,ITOCD(4,I),LKYNM_chr,NVER,ILAST,MAXTOC, &
     &          IDCBLC_un(1),CONT,dbhsg_format )
!
!     SEE IF WE HAVE EXHAUSTED THE DUMP
!
      IF(IQUIT(IDUMP,I,ITOCD(3,I),ITOCD(4,I)) .EQ. 1) GO TO 700
690   CONTINUE
      GO TO 682
!
! 7.    THE END
!
700   CONTINUE
      CALL FINIS( INT2(0) )
710   CONTINUE
      WRITE(ILUOUT,720)
720   FORMAT(1X,"END OF DUMP")
      CLOSE(unit=IDCBLC_un(1),status='DELETE',IOSTAT=IOS)
      IF (ILUOUT.EQ.65) CLOSE (UNIT=ILUOUT,STATUS='KEEP')
      END
      integer*2 FUNCTION IQUIT(IDUMP,I,IWANT,IHAVE)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!       A FUNCTION TO DETERMINE IF WE HAVE DUMPED ALL OF THE RECORDS
!       THAT WE WANT TO. = 1 IF WE ARE FINISHED.  = 0 OTHERWISE.
!
      integer*2 IMDEAD(100),idump,i,iwant,ihave
      DATA IMDEAD /100*0/
      INTEGER*4  IOS
!
!
!       BRUCE SCHUPLER  -  3 OCTOBER 1979
!
!# LAST COMPC'ED  830208:18:36                          #
!
! 1.    INITIALIZE
!
      IQUIT = 1
      IF((IHAVE .GE. IWANT) .AND. (IWANT .NE. 0)) IMDEAD(I) = 1
!
! 2.    SEE IF IDUMP ELEMENTS OF IMDEAD ARE MARKED AS 1
!
      DO 2000 I=1,IDUMP
      IF(IMDEAD(I) .EQ. 1) GO TO 2000
      IQUIT = 0
2000  CONTINUE
!
! 3.    THE END
!
      RETURN
      END
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DTOC(ILUOUT,ITOC,I,ILAST,MAXTOC,LKYNM_chr,NVER, &
     &      IDCBLC_un,dbhsg_format)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  IOS
!
!       SUBROUTINE TO DUMP THE CONTENTS OF TOC ITOC(1,I)
!
      integer*2 LTXT(16),NDIM(3),IBUF(40),ITOC(2,*), &
     &    IDCBLC_un(16)
      integer*2 irec,iluout,i,ilast,maxtoc,nver,ierr,j,imany
      integer*2 ntot,k,mtype,iadver
      CHARACTER*80 IBUFC
      character*10 lkynm_chr
      EQUIVALENCE (IBUF,IBUFC)
      integer*2 LCODE(4),LTYPE(5)
      CHARACTER*1 CONT
      INTEGER*2 SCRNLNS
      CHARACTER*10 LTYPE_CHR
      EQUIVALENCE (LTYPE_CHR,LTYPE)
      logical*2 dbhsg_format
      DATA LTYPE_CHR /'R I A D J '/
!
!       BRUCE SCHUPLER  -  3 OCTOBER 1979
!       BRUCE SCHUPLER  -  811027
!     :93.12.29:kdb: Fix range error (ltype mis-dimensioned).  (Could not
!                    handle integer*4 type.)
!
! 1.    POSITION THE LCODE FILE PROPERLY TO BEGIN
!
!      CALL READF(IDCBLC_un(1),IERR,IBUF,40,LEN,ITOC(2,I))
      irec = itoc(2,I)
      READ(unit=IDCBLC_un(1),iostat=IOS,rec=irec)(IBUF(j),j=1,40)
      IERR = IOS
!
! 2.    SEE HOW MANY ENTRIES THERE ARE IN THIS TOC
!
      IMANY = ITOC(2,I+1) - ITOC(2,I) - 1
      IF((I .EQ. MAXTOC) .OR. (ITOC(2,I+1) .EQ. &
     &      0))IMANY = ILAST - ITOC(2,I) - 1
!
! 3.    SAY HOWDY
!
      SCRNLNS=0
!     Initialize continue option
      CONT='Y'
      WRITE(ILUOUT,300) ITOC(1,I),LKYNM_chr,NVER
300   FORMAT(//,1X,"Table of Contents for TOC type ",I3," of data base ", &
     &          a10," Version ",I3,//,1X, &
     &"      LCODE      DIMENSIONS TYPE ADDED AT      DESCRIPTOR ",/, &
     &28X,"VERSION",/)
      SCRNLNS=SCRNLNS+5
!
! 4.    DUMP THE TOC
!
      NTOT = 0
      DO 490 J=1,IMANY
      CALL BLANK( IBUF, INT2(40) )
!      CALL READF(IDCBLC_un(1),IERR,IBUF,40,LEN)
      irec = irec+1
      READ(unit=IDCBLC_un(1),iostat=IOS,rec=irec)(IBUF(k),k=1,40)
      IERR = IOS
      READ(IBUFC,400) MTYPE,LCODE,IADVER,NDIM,LTXT
400   FORMAT(I1,1X,4A2,4I4,1X,16A2)
      if(dbhsg_format) then
        write(iluout, &
     &  '(5i3,1x,4a2,1x,16a2)')itoc(1,i), mtype, ndim, lcode,ltxt
      else
        WRITE(ILUOUT,410) J,LCODE,NDIM,LTYPE(MTYPE),IADVER,LTXT
410     FORMAT(1X,I3,". ",4A2,2X,3(I3,1X),2X,A1,4X,I3,5X,16A2)
      endif
      SCRNLNS=SCRNLNS+1
!     Test for number of lines on screen.
      IF ((ILUOUT.EQ.6).AND.(SCRNLNS.GT.22).AND.(CONT.NE.'S')) THEN
      WRITE (ILUOUT,8000)
 8000   FORMAT ("Continue listing ((Y)/N/S to stop asking)? ",$)
      READ (5,'(A1)') CONT
      SCRNLNS=0
      CALL CASEFOLD(CONT )
      IF (CONT.EQ.'N') GO TO 500
      ENDIF
490   CONTINUE
!
! 5.    THE END
!
 500  RETURN
      END
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DREC(ILUOUT,ITOC,IREC,IHAVE,LKYNM_chr,NVER,ILAST, &
     &      MAXTOC,IDCBLC_un,CONT,dbhsg_format)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  IOS
!
!       SUBROUTINE TO DUMP THE CONTENTS OF TOC ITOC(1,I) ON LU
!       ILUOUT
!
!.  Include now used for: dbuc, jbuc, lbuc, rbuc.
      INCLUDE 'dbase96.i'
!
      integer*2 iluout,irec,ihave,nver,ilast,maxtoc,i,ipoint,j
      integer*2 ierr,iflag,imany,mtype,kerr,jflag, nn,jj
!.  Old "ibuf" now "ibufi".
      integer*2 ITOC(2,*),IDCBLC_un(16),ibufi(40),NDIM(3), &
     &          LCODE(7),NDO(3)
!
      integer*2 lbuc(mbuf32*16)
      real*8    rbuc(mbuf32*4)
      integer*4 jbuc(mbuf32*8)
      real*8    dbuc(mbuf32*4)
      CHARACTER*80 IBUFC
      character*10 LKYNM_chr
      character*14 lcode_chr
      equivalence(lcode_chr,lcode(1))
      EQUIVALENCE (ibufi,IBUFC)
      integer*2 inum
      INTEGER*2 SCRNLNS
      CHARACTER*1 CONT
      logical*2 dbhsg_format
!.    EQUIVALENCE (        lbuc(1),dbuc(1),jbuc(1))
!     EQUIVALENCE (rbuc(1),lbuc(1),dbuc(1),jbuc(1))
!
!       BRUCE SCHUPLER  -  5 OCTOBER 1979
!       BRUCE SCHUPLER  -  811027
!  BA  95.10.19  Main lcode buffers (lbuc, etc.) obtained from include.
!  kdb 97.08.27  Fix error so that "Stop asking" stops asking for the
!                entire dump, not just until the next record,
!                (the standard meaning of stop asking).
!                At the same time add a "Remainder of record" feature
!                to replace the feature erased by the fix. (This dumps
!                the remainder of the record, then solicits further
!                instructions.)
!
! 0.   PRINT A MESSAGE
!
      IF(CONT.EQ.'R') CONT = 'Y'
      WRITE(ILUOUT,50) LKYNM_chr,NVER,IHAVE,IREC
50    FORMAT(1X,"Data base ",a10," Version ",I3," Occurrence ",I5, &
     &             " of record type ",I3,/)
!
! 1.    FIND THE RECORD POINTER IN ITOC AND POSITION THE LCODE FILE CORRECTLY.
!       ALSO, READ THE FLAG THAT INDICATES WHETHER OR NOT WE ARE DUMPING
!       ALL OF THE ENTRIES
!
      DO 100 I=1,MAXTOC
      IPOINT = I
      IF(ITOC(1,I) .EQ. IREC) GO TO 110
100   CONTINUE
110   CONTINUE
!      CALL READF(IDCBLC_un(1),IERR,ibufi,40,LEN,ITOC(2,IPOINT))
      inum = itoc(2,ipoint)
      READ(unit=IDCBLC_un(1),iostat=IOS,rec=inum)(ibufi(i),i=1,40)
      IERR = IOS
      IFLAG = ibufi(12)
!
! 2.    CALCULATE HOW MANY ENTRIES THERE ARE IN THIS TOC
!
      IMANY = ITOC(2,IPOINT + 1) - ITOC(2,IPOINT) - 1
      IF((IPOINT .EQ. MAXTOC) .OR. (ITOC(2,IPOINT+1) .EQ. &
     &      0))IMANY = ILAST - ITOC(2,IPOINT) - 1
!
! 3.   READ THE LCODE AND SEE IF WE DUMP IT
!
      DO 350 J=1,IMANY
      inum = inum+1
      READ(unit=IDCBLC_un(1),iostat=IOS,rec=inum) (ibufi(i),i=1,40)
      IERR = IOS
      READ(IBUFC,300) MTYPE,LCODE_chr,NDIM,JFLAG
300   FORMAT(I1,1X,A8,4X,3I4,34X,A1)
      IF((IFLAG .EQ.'S') .AND. (JFLAG .NE. '1')) GO TO 350
      LCODE(5) = 0
      LCODE(6) = 0
      LCODE(7) = 0
!
! 4.    DO THE GET
!
      IF(MTYPE .EQ. 1) CALL GETR(LCODE_chr,rbuc,NDIM(1),NDIM(2),NDIM(3), &
     &                           NDO,KERR)
      IF(MTYPE .EQ. 2) CALL GETI(LCODE_chr,lbuc,NDIM(1),NDIM(2), &
     &                           NDIM(3),NDO,KERR)
      IF(MTYPE .EQ. 3) CALL GETA(LCODE_chr,lbuc,NDIM(1),NDIM(2), &
     &                           NDIM(3),NDO,KERR)
      IF(MTYPE .EQ. 4) CALL GETR(LCODE_chr,dbuc,NDIM(1),NDIM(2), &
     &                           NDIM(3),NDO,KERR)
      IF(MTYPE .EQ. 5) CALL GETJ(LCODE_chr,jbuc,NDIM(1),NDIM(2), &
     &                           NDIM(3),NDO,KERR)
!
!
! 5.    DO THE PRINTING
!
      if(.not.dbhsg_format) then
        IF(MTYPE .EQ. 1)CALL PRINR(LCODE_chr,rbuc,NDIM,ILUOUT,CONT, &
     &     SCRNLNS)
        IF(MTYPE .EQ. 2)CALL PRINI(LCODE_chr,lbuc,NDIM,ILUOUT,CONT, &
     &     SCRNLNS)
        IF(MTYPE .EQ. 3)CALL PRINA(LCODE_chr,lbuc,NDIM,ILUOUT,CONT, &
     &     SCRNLNS)
        IF(MTYPE .EQ. 4)CALL PRIND(LCODE_chr,dbuc,NDIM,ILUOUT,CONT, &
     &     SCRNLNS)
        IF(MTYPE .EQ. 5)CALL PRINJ(LCODE_chr,jbuc,NDIM,ILUOUT,CONT, &
     &     SCRNLNS)
      else
        nn = ndim(1)*ndim(2)*ndim(3)
        write(iluout,'(5i3,1x,a8)') i,mtype,ndim,lcode_chr
        if(mtype.eq.1) write(iluout,'(3(d22.16,1x))')(rbuc(jj),jj=1,nn)
        if(mtype.eq.2) write(iluout,'(10i7)       )')(lbuc(jj),jj=1,nn)
        if(mtype.eq.3) write(iluout,'(40a2)       )')(lbuc(jj),jj=1,nn)
        if(mtype.eq.4) write(iluout,'(3(d22.16,1x))')(rbuc(jj),jj=1,nn)
        if(mtype.eq.5) write(iluout,'(10i7)       )')(jbuc(jj),jj=1,nn)
      endif
!
      IF (CONT.EQ.'N') GO TO 355
350   CONTINUE
!
! 6.    THE END
!
 355  RETURN
      END
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PRINR(LCODE_chr,rbuc,NDIM,ILUOUT,CONT,SCRNLNS)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  IOS
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 ndim, iluout, i, i3, i2, isum, i1
!-----END of imp added lines.
!
!                                             ,C#830208:18:36#
!
!       PRINT REAL NUMBERS
!
      DIMENSION rbuc(*),NDIM(*)
      DOUBLE PRECISION rbuc
      character*8 lcode_chr
      CHARACTER*1 CONT
      INTEGER*2 SCRNLNS
!
!     BRUCE SCHUPLER  5 OCTOBER 1979
!     kdb 970827 Add a "Remainder of record" feature to dump
!                the remainder of the record, then solicit further
!                instructions.
!
! 1.    WRITE THE LCODE AND DIMENSIONS
!
      WRITE(ILUOUT,100) LCODE_chr,(NDIM(I),I=1,3)
100   FORMAT(1X,"Lcode = ",a8,"   Dimensions = (",3(I3,1X),")")
      SCRNLNS=SCRNLNS+3
!
! 2.    WRITE THE ARRAY OUT
!
      DO 220 I3=1,NDIM(3)
        DO 210 I2=1,NDIM(2)
        ISUM = (I2-1)*NDIM(1) + (I3-1)*NDIM(2)*NDIM(1)
        DO 150, I1=1,NDIM(1)
!         Test for number of lines on screen.
          IF ((ILUOUT.EQ.6).AND.(SCRNLNS.GT.23).AND. &
     &                  (CONT.NE.'R' .AND. CONT.NE.'S')) THEN
            WRITE (ILUOUT,8000)
 8000       FORMAT ("Continue listing ", &
     &         "((Y)/N/Remainder of rec/Stop asking)? ",$)
            READ (5,'(A1)') CONT
            SCRNLNS=0
            CALL CASEFOLD(CONT )
            IF (CONT.EQ.'N') GO TO 230
          ENDIF
          WRITE(ILUOUT,200) rbuc(ISUM + I1)
          IF ((I1/3*3).EQ.I1) THEN
            WRITE (ILUOUT,145)
 145        FORMAT(1X)
            SCRNLNS=SCRNLNS+2
          ENDIF
 150    CONTINUE
!200     FORMAT(2(1X,D30.16))
!200     FORMAT(3(1X,D25.13))
200     FORMAT(D26.16,$)
        WRITE(ILUOUT,205)
205     FORMAT(/)
        SCRNLNS=SCRNLNS+2
210     CONTINUE
220   CONTINUE
!
! 3.    THAT'S ALL
!
 230  RETURN
      END
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PRINI(LCODE_chr,lbuc,NDIM,ILUOUT,CONT,SCRNLNS)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  IOS
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 lbuc, ndim, iluout, i, i3, i2, isum, i1
!-----END of imp added lines.
!
!                                             ,C#830208:18:36#
!
!       PRINT INTEGERS
!
      DIMENSION lbuc(*),NDIM(*)
      character*8 lcode_chr
      CHARACTER*1 CONT
      INTEGER*2 SCRNLNS
!
!       BRUCE SCHUPLER  5 OCTOBER 1979
!
!     kdb 970827 Add a "Remainder of record" feature to dump
!                the remainder of the record, then solicit further
!                instructions.
!
! 1.    WRITE THE LCODE AND DIMENSIONS
!
      WRITE(ILUOUT,100) LCODE_chr,(NDIM(I),I=1,3)
100   FORMAT(1X,"Lcode = ",a8," Dimensions = (",3(I3,1X),")")
      SCRNLNS=SCRNLNS+3
!
! 2.    WRITE THE ARRAY OUT
!
      DO 220 I3=1,NDIM(3)
        DO 210 I2=1,NDIM(2)
        ISUM = (I2-1)*NDIM(1) + (I3-1)*NDIM(2)*NDIM(1)
        DO 150, I1=1,NDIM(1)
!         Test for number of lines on screen.
          IF ((ILUOUT.EQ.6).AND.(SCRNLNS.GT.22).AND. &
     &                  (CONT.NE.'R'.AND.CONT.NE.'S')) THEN
            WRITE (ILUOUT,8000)
 8000       FORMAT ("Continue listing ", &
     &         "((Y)/N/Remainder of rec/Stop asking)? ",$)
            READ (5,'(A1)') CONT
            SCRNLNS=0
            CALL CASEFOLD(CONT )
            IF (CONT.EQ.'N') GO TO 230
          ENDIF
          WRITE(ILUOUT,200) lbuc(ISUM + I1)
          IF ((I1/11*11).EQ.I1) THEN
            WRITE (ILUOUT,205)
            SCRNLNS=SCRNLNS+2
          ENDIF
 150    CONTINUE
200     FORMAT((1X,I6),$)
        WRITE(ILUOUT,205)
205     FORMAT(/)
        SCRNLNS=SCRNLNS+2
210     CONTINUE
220   CONTINUE
!
! 3.    THAT'S ALL
!
 230  RETURN
      END
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PRIND(LCODE_chr,dbuc,NDIM,ILUOUT,CONT,SCRNLNS)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  IOS
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 ndim, iluout, i, i3, i2, isum, i1
!-----END of imp added lines.
!
!                                             ,C#830208:18:36#
!
!       PRINT REAL*8 NUMBERS
!
      DIMENSION dbuc(*),NDIM(*)
      REAL*8 dbuc
      character*8 lcode_chr
      CHARACTER*1 CONT
      INTEGER*2 SCRNLNS
!
!     BRUCE SCHUPLER  5 OCTOBER 1979
!     BRUCE SCHUPLER  811027
!
!     kdb 970827 Add a "Remainder of record" feature to dump
!                the remainder of the record, then solicit further
!                instructions.
!
! 1.    WRITE THE LCODE AND DIMENSIONS
!
      WRITE(ILUOUT,100) LCODE_chr,(NDIM(I),I=1,3)
100   FORMAT(1X,"Lcode = ",a8,"   Dimensions = (",3(I3,1X),")")
      SCRNLNS=SCRNLNS+3
!
! 2.    WRITE THE ARRAY OUT
!
      DO 220 I3=1,NDIM(3)
        DO 210 I2=1,NDIM(2)
        ISUM = (I2-1)*NDIM(1) + (I3-1)*NDIM(2)*NDIM(1)
        DO 150, I1=1,NDIM(1)
!         Test for number of lines on screen.
          IF ((ILUOUT.EQ.6).AND.(SCRNLNS.GT.22).AND. &
     &                  (CONT.NE.'R'.AND.CONT.NE.'S')) THEN
            WRITE (ILUOUT,8000)
 8000       FORMAT ("Continue listing ", &
     &         "((Y)/N/Remainder of rec/Stop asking)? ",$)
            READ (5,'(A1)') CONT
            SCRNLNS=0
            CALL CASEFOLD(CONT )
            IF (CONT.EQ.'N') GO TO 230
          ENDIF
          WRITE(ILUOUT,200) dbuc(ISUM + I1)
        IF ((I1/2*2).EQ.I1) THEN
          WRITE (ILUOUT,205)
          SCRNLNS=SCRNLNS+2
        ENDIF
 150    CONTINUE
200     FORMAT(D26.16,$)
! 200     FORMAT(2(1X,D30.16))
        WRITE(ILUOUT,205)
205     FORMAT(/)
        SCRNLNS=SCRNLNS+2
210     CONTINUE
220   CONTINUE
!
! 3.    THAT'S ALL
!
 230  RETURN
      END
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PRINJ(LCODE_chr,jbuc,NDIM,ILUOUT,CONT,SCRNLNS)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  IOS
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 ndim, iluout, i, i3, i2, isum, i1
!-----END of imp added lines.
!
!                                             ,C#830208:18:36#
!
!       PRINT INTEGER*4
!
      DIMENSION jbuc(*),NDIM(*)
      INTEGER*4 jbuc
      character*8 lcode_chr
      CHARACTER*1 CONT
      INTEGER*2 SCRNLNS
!
!     BRUCE SCHUPLER  5 OCTOBER 1979
!     BRUCE SCHUPLER  811027
!
!     kdb 970827 Add a "Remainder of record" feature to dump
!                the remainder of the record, then solicit further
!                instructions.
!
! 1.    WRITE THE LCODE AND DIMENSIONS
!
      WRITE(ILUOUT,100) LCODE_chr,(NDIM(I),I=1,3)
100   FORMAT(1X,"Lcode = ",a8," Dimensions = (",3(I3,1X),")")
      SCRNLNS=SCRNLNS+3
!
! 2.    WRITE THE ARRAY OUT
!
      DO 220 I3=1,NDIM(3)
        DO 210 I2=1,NDIM(2)
        ISUM = (I2-1)*NDIM(1) + (I3-1)*NDIM(2)*NDIM(1)
      DO 150, I1=1,NDIM(1)
!         Test for number of lines on screen.
          IF ((ILUOUT.EQ.6).AND.(SCRNLNS.GT.22).AND. &
     &                  (CONT.NE.'R'.AND.CONT.NE.'S')) THEN
          WRITE (ILUOUT,8000)
 8000     FORMAT ("Continue listing ", &
     &         "((Y)/N/Remainder of rec/Stop asking)? ",$)
          READ (5,'(A1)') CONT
          SCRNLNS=0
          CALL CASEFOLD(CONT )
          IF (CONT.EQ.'N') GO TO 230
          ENDIF
          WRITE(ILUOUT,200) jbuc(ISUM + I1)
        IF ((I1/6*6).EQ.I1) THEN
          WRITE (ILUOUT,205)
          SCRNLNS=SCRNLNS+2
        ENDIF
 150    CONTINUE
!C        WRITE(ILUOUT,200) (jbuc(ISUM + I1),I1=1,NDIM(1))
200     FORMAT((1X,I11),$)
        WRITE(ILUOUT,205)
205     FORMAT(/)
        SCRNLNS=SCRNLNS+2
210     CONTINUE
220   CONTINUE
!
! 3.    THAT'S ALL
!
 230  RETURN
      END
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PRINA(LCODE_chr,LBUF,NDIM,ILUOUT,CONT,SCRNLNS)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  IOS
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 lbuf, ndim, iluout, i, i3, i2, isum, i1
!-----END of imp added lines.
!
!                                             ,C#830208:18:36#
!
!       PRINT REAL NUMBERS
!
      DIMENSION LBUF(*),NDIM(*)
      character*8 lcode_chr
      CHARACTER*1 CONT
      INTEGER*2 SCRNLNS
!
!      BRUCE SCHUPLER  5 OCTOBER 1979
!
!     kdb 970827 Add a "Remainder of record" feature to dump
!                the remainder of the record, then solicit further
!                instructions.
!
! 1.    WRITE THE LCODE AND DIMENSIONS
!
      WRITE(ILUOUT,100) LCODE_chr,(NDIM(I),I=1,3)
100   FORMAT(1X,"Lcode = ",a8," Dimensions = (",3(I3,1X),")")
      SCRNLNS=SCRNLNS+3
!
! 2.    WRITE THE ARRAY OUT
!
      DO 220 I3=1,NDIM(3)
        DO 210 I2=1,NDIM(2)
        ISUM = (I2-1)*NDIM(1) + (I3-1)*NDIM(2)*NDIM(1)
        DO 150, I1=1,NDIM(1)
!        Test for number of lines on screen.
         IF ((ILUOUT.EQ.6).AND.(SCRNLNS.GT.22).AND. &
     &                  (CONT.NE.'R'.AND.CONT.NE.'S')) THEN
            WRITE (ILUOUT,8000)
 8000       FORMAT ("Continue listing ", &
     &         "((Y)/N/Remainder of rec/Stop asking)? ",$)
            READ (5,'(A1)') CONT
            SCRNLNS=0
            CALL CASEFOLD(CONT )
          IF (CONT.EQ.'N') GO TO 230
         ENDIF
          WRITE(ILUOUT,200) LBUF(ISUM + I1)
        IF ((I1/38*38).EQ.I1) THEN
          WRITE (ILUOUT,205)
          SCRNLNS=SCRNLNS+2
        ENDIF
 150    CONTINUE
200     FORMAT(A2,$)
        WRITE(ILUOUT,205)
205     FORMAT(1X)
        SCRNLNS=SCRNLNS+2
210     CONTINUE
220   CONTINUE
!
! 3.    THAT'S ALL
!
 230  RETURN
      END
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DBHBS(LKYNM_chr,IVER,iLU_stdout,IDLC_un,LQ,NQ, &
     &             ILAST,KERR)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  IOS,INN
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 iver, ilu_stdout, idlc_un, lq, nq, ilast, kerr
      INTEGER*2 icr, ierr, llen, iverc, j, idum, itoc, irec
      INTEGER*2 istop, ntoc, i, next, k, iea, m, it, ityp, ii
      INTEGER*2 IDUM_BUFFER(500)
!-----END of imp added lines.
!
!     1C#830208:18:36#
!
!     DBHBS
!
! 1.  DBHBS PROGRAM SPECIFICATION
!
! 1.1.   DBHBS IS A SUBROUTINE WHICH GENERATES AN ASCII FILE OF
!        LCODES, DIMENSIONS, AND DISCRIPTORS FROM DATA BASE. THE
!        USER MUST PASS THE DCB BUFFER OF AN OPEN TYPE 3 FILE WHICH
!        THIS PROGRAM WILL LOAD WITH THE LCODE INFORMATION.  UPON
!        COMPLETION THIS ROUTINE WILL REWIND THE LCODE FILE.
!
! 1.2.   RESTRICTIONS - limits on use of routine
!
! 1.3.   REFERENCES - document cited
!
! 2.  DBHBS INTERFACE
!
! 2.1.   CALLING SEQUENCE: CALL DBHBS(LKYNM_chr,IVER,LU,IDLC_un,LQ,NQ,ILAST,
!                                     KERR)
!
!     INPUT VARIABLES:
!
      DIMENSION LQ(2,*),IDLC_un(16)
!
!      LKYNM_chr - THE NAME OF THE DATA BASE TO BE UPDATED
!      IVER      - THE VERSION NUMBER OF THE DATA BASE TO BE UPDATED
!      LU        - UNIT FOR ERROR MESSAGES.
!      IDLC_un(1) - THE UNIT NUMBER FOR THE LCODE FILE.
!      NQ        - THE SECOND DIMENSION OF THE ARRAY LQ.
!
!     OUTPUT VARIABLES:
!
!       LQ(2,-) - AN ARRAY OF TOC POINTS. FOR EACH ROW IN LQ THE FIRST
!               ELEMENT CONTAIN A DATA RECORD TYPE NUMBER AND THE SECOND
!               ELEMENT CONTAINS THE RECORD NUMBER IN THE LCODE FILE WHERE
!               THE TABLE OF CONTENTS FOR THAT DATA RECORD TYPE BEGINS.
!       ILAST - THE RECORD NUMBER OF THE LAST RECORD IN THE LCODE FILE.
!       KERR - ERROR FLAG ( 0 - NORMAL EXIT,  -1 - THE DATA BASE WAS
!              NOT ACTIVE OR DID NOT EXIT, -2 - COULD NOT OPEN THE
!              FILE #LCODE::22)
!
! 2.5.   SUBROUTINE INTERFACE:
!
!     CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INCLUDE  'dbase96.i'
      integer*2 LTXT(16,toc_length)
!
      integer*2 LFMGR(7),IDDB_un(16), &
     & qbuf(800),jbufi(100)
      character*157 fpath,AREA_NAME
      INTEGER*2 NAME_LEN
      character*160 qbuf_chr
      CHARACTER*200 jbufc
      character*10 lkynm_chr, lkynm2_chr
      integer*2 lkynm(5),getunit
      integer*2 dbunit, ihold, ibuffer(100)
      common /leftmp/ dbunit
      equivalence (lkynm2_chr,lkynm(1))
      EQUIVALENCE (jbufi,jbufc)
      EQUIVALENCE (qbuf,LTXT), (qbuf(1),qbuf_chr)

! 6.  PROGRAMMER: JIM RYAN
!                 BRUCE R. SCHUPLER 811027
!
!     PROGRAM STRUCTURE
!
!     1. GET THE FILE MANAGER NAME AND SECURITY CODE OF
!        THE DATA BASE FILE FROM THE CATALOG.
 
 
       CALL open_mk3_cat(2HSB,KERR )
       IF(KERR.NE.0) STOP 1
       lkynm2_chr = lkynm_chr
       CALL name_to_path(LKYNM,IVER,LFMGR,ICR,FPATH,KERR )
       CALL close_cat()
       IF (KERR.NE.0  .OR. ICR.EQ.0) then
         go to 1
       else
         go to 2
       endif

!     THEN BEGIN couldn't find the data base error return

  1     KERR = -1
       GO TO 32767
!     ENDT couldn't find the data base error return
!
!     2. OPEN THE FILE, CHECK THAT THE NAME AND VERSION NUMBER ARE
!        CORRECT, THEN POSTION THE FILE AFTER THE HISTORY TERMINATION
!        RECORD.
!
  2     CONTINUE
!
!      CALL OPEN(IDDB,IERR,LFMGR,1,ISEC,ICR)
       iddb_un(1) = getunit()

       CALL RSV_SWITCH( 'L', ICR, AREA_NAME, NAME_LEN, INT2(0), IERR )
       IF(IERR.EQ.-1) THEN
       WRITE(ilu_stdout,'("You broke and DBHBS could not open the ", &
      &   "data base file",/,A80,/)')fpath(1:80)
       STOP
       ELSEIF(IERR.EQ.-2) THEN
       WRITE(ilu_stdout,'("You requested a platter which does not ",$, &
      &   "exist and thus DBHBS could not",//, &
      &   "open the data base file",/, A80,/)')fpath(1:80)
       STOP
       ENDIF
       



#ifdef LINUX
      open(unit=iddb_un(1),file=fpath,status='OLD', &
     &     form='BINARY',iostat=ios)
#else
      open(unit=iddb_un(1),file=fpath,status='OLD', &
     &     form='UNFORMATTED',iostat=ios)
#endif
      IERR = IOS
      IF(IERR.GT.0) then
        write(ilu_stdout,'("DBHBS open error ",i5, &
     &                        " on database file ",/,A70)')ierr, &
     &                          fpath(1:70)
        STOP 2
      endif
      rewind dbunit
!
      CALL READZ( IDDB_un(1), IERR, qbuf, INT2(7), int2(1) )
      CALL READZ( IDDB_un(1), IERR, qbuf, INT2(5), int2(2) )
      CALL READZ( IDDB_un(1), IERR, qbuf, INT2(40),int2(3) )
      CALL READZ( IDDB_un(1), IERR, qbuf, INT2(5), int2(4) )
! Check the data base name; stop if it doesn't agree

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Commented out for testing purposes
!
       IF (qbuf_chr(1:10).NE.LKYNM_chr(1:10)) then
         CLOSE(unit=IDDB_un(1),iostat=IOS)
         IERR = IOS
       CALL RSV_SWITCH( 'U', ICR, AREA_NAME, NAME_LEN, INT2(0), IERR )
       STOP "DBHD3/DBHBS STOP 3: Database name disagreement"
       endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!

      CALL READZ( IDDB_un(1), IERR, IVERC, INT2(1), int2(5) )

! Check for bad version number; stop if error
! Commented out for testing purposes

       IF (IVER.NE.0 .AND. IVER.NE.IVERC) then
         CLOSE(unit=IDDB_un(1),iostat=IOS)
         IERR = IOS
       CALL RSV_SWITCH( 'U', ICR, AREA_NAME, NAME_LEN, INT2(0), IERR )
         STOP "	DBHD3/DBHBS 4: Database version disagreent"
       endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      CALL READZ( IDDB_un(1), IERR, qbuf, INT2(7), int2(6) )
      CALL READZ( IDDB_un(1), IERR, qbuf, INT2(16),int2(7) )
!
      jbufc = ' '
      WRITE(jbufc,2001) LKYNM_chr, IVERC
2001  FORMAT("DATA BASE ",a10,"  VERSION ",I6)
      WRITe(unit=IDLC_un(1),iostat=IOS,rec=1)jbufc(1:80)
      IERR = IOS
!
!      NOW READ PAST THE HISTORY RECORDS
!
      JBUFI(1) = 0
! Look for the history termination record
12    IF (JBUFI(1).eq.2HZZ) go to 14
        CALL READZ( IDDB_un(1), IERR, Jbufi, INT2(12), INT2(7) )
        IF(JBUFI(1).NE.2HZZ) THEN
          CALL READZ( IDDB_un(1), IERR, ibuffer, INT2(12), INT2(7) )
          ihold = jbufi(2)
          CALL READZ( IDDB_un(1), IERR, IDUM_BUFFER,  ihold   , INT2(3) )
        ENDIF
        GO TO 12
14    continue
!
!     4. NOW READ IN THE TOCS AND DUMP OUT THE LCODE.
!
      ITOC = 0
      IREC = 1
      ISTOP = 0
!
!  The following really needs cleaning up!!!  I will do it after the next
!  version of DBHD3 is running.  - lef
!
15    IF (ISTOP.EQ.0) then
      go to 16
      else
      go to 17
      endif
!  DO BEGIN read in toc flag, toc extent flags, toc , and text records
!         read the toc flag record.
16        CALL READZ( IDDB_un(1), IERR, qbuf, INT2(12), INT2(7))
            IF (qbuf(1).EQ.2HZZ) then
            go to 18
          else
            go to 19
          endif
!             THEN BEGIN quite because toc termination record found
18              ISTOP = 1
!               ENDT quite because toc termination record found
              GO TO 20
!             ELSE BEGIN another toc flag record found
19              NTOC = qbuf(2)
                jbufc = ' '
                ITOC = ITOC+1
                IF (ITOC.GT.NQ) then
!!!!!!!!!!!!!!!!!!!!!!!!!!
!Commented out for testing
                 CALL RSV_SWITCH( 'U', ICR, AREA_NAME, NAME_LEN, INT2(0), IERR )
!!!!!!!!!!!!!!!!!!!!!!
                go to 22
              else
                      go to 23
                    endif
22                  STOP 3001
23                  CONTINUE
                IREC = IREC+1
                LQ(1,ITOC) = NTOC
                LQ(2,ITOC) = IREC
                WRITE(jbufc,3001) NTOC
 3001           FORMAT("** RECORD TYPE ",I5)
!                CALL WRITF(IDLC_un(1),IERR,jbufi,40,IREC)
                WRITe(unit=IDLC_un(1),iostat=IOS, &
     &                   rec=IREC)(jbufi(i),i=1,40)
                IERR = IOS
                NEXT = qbuf(3)
                DO    25 K=1,NEXT
!                 DO BEGIN read toc ext. flag rec, toc, and text recs
!                    
!                 read the toc extent flag record. 
                    CALL READZ( IDDB_un(1), IERR, qbuf, INT2(12), INT2(7) )
!
!     3 VARIABLE TYPE OR 5 VARIABLE TYPE TOC
!
                    llen = 12
                    IF(llen .EQ. 8) IEA = qbuf(5)
                    IF(llen .EQ. 12) IEA = qbuf(10)
                    ihold = qbuf(10)*8
                    CALL READZ(IDDB_un(1),IERR,LTOC,ihold,INT2(8) )
                    ihold = qbuf(10)*16
                    CALL READZ(IDDB_un(1),IERR,LTXT,ihold,INT2(9) )
                    DO    26 M=1,IEA
!                     DO BEGIN dump out the lcode lines
                         IT = LTOC(1,M)
                         IF(LTOC(2,M) .NE. 2HFI) GO TO 27
                         IF(LTOC(3,M) .NE. 2HLL) GO TO 27
                         IF(LTOC(4,M) .NE. 2HER) GO TO 27
                         IF(IT.EQ.2HR-) ITYP = 1
                         IF(IT.EQ.2HI-) ITYP = 2
                         IF(IT.EQ.2HA-) ITYP = 3
                         IF(IT.EQ.2HD-) ITYP = 4
                         IF(IT.EQ.2HJ-) ITYP = 5
                         IF (IT.NE.2HR- .AND. IT.NE.2HI- .AND. &
     &                   IT.NE.2HA- .AND. IT.NE.2HD- .AND. &
     &                   IT.NE.2HJ-) then
                        go to 27
                      else
                        go to 28
                      endif
!                          THEN BEGIN not a filler so dump
27                           DO    30 I=1,40
                                 jbufi(I) = 2H  !blanks
30                               CONTINUE
                             WRITE(jbufc,3002) ITYP,(LTOC(II,M),II=1,8), &
     &                       (LTXT(II,M),II=1,16)
 3002                        FORMAT(I1,1X,4A2,4I4,1X,16A2)
                             IREC = IREC+1
!                            CALL WRITF(IDLC_un(1),IERR,jbufi,40,IREC)
                             WRITe(unit=IDLC_un(1),iostat=IOS, &
     &                   rec=IREC)(jbufi(ii),ii=1,40)
                             IERR = IOS
                             IF(IERR.gT.0) STOP 3000
!                            ENDT not a filler so dump
28                           CONTINUE
!                       ENDF dump out the lcode lines
26                      CONTINUE
!                   ENDF read toc ext. flag rec, toc, and text recs
25                  CONTINUE
!               ENDE another toc flag record found
20              CONTINUE
!         ENDW read in toc flag, toc extent flags, toc , and text records
          GO TO 15
17        CONTINUE
!
      DO    31 I=1,40
          jbufi(I) = 2H  !blanks
31        CONTINUE
      jbufi(1) = 2H##
      IREC = IREC+1
      ILAST = IREC
      WRITe(unit=IDLC_un(1),iostat=IOS,rec=IREC)(jbufi(i),i=1,40)
      IERR = IOS
!
!     5. CLOSE UP THE DATA BASE FILE AND REWIND THE LCODE FILE.
!
      CLOSE(unit=IDDB_un(1),iostat=IOS)
      IERR = IOS
      rewind(unit=IDLC_un(1),iostat=IOS)
      IERR = IOS

!!!!!!!!!!!!!!!!!!!!!!!!!
!Commented out for testing
       CALL RSV_SWITCH( 'U', ICR, AREA_NAME, NAME_LEN, INT2(0), IERR )
!!!!!!!!!!!!!!!!!!!!!!!!!!!

      KERR = 0
      RETURN
32767 RETURN
      END
!
! ------------------------------------------------------------------------
!
      SUBROUTINE WHICH(IREC,ITOC,ilu_stdin,ILU_stdout,IDCBLC_un, &
     &                  MAXTOC,ILAST)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  IOS
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 irec, itoc, ilu_stdin, ilu_stdout, idcblc_un
      INTEGER*2 maxtoc, ilast, i, ipoint, ierr, ibuf, imany
      INTEGER*2 lcode, k, j
!-----END of imp added lines.
!
!     1C#830208:18:36#
!
!     WHICH WILL CHECK WITH THE USER TO SEE IF HE WANTS TO DUMP ALL
!     OF THE ITEMS IN A RECORD OR JUST SOME OF THEM.  IF WE ARE TO DUMP
!     JUST SOME OF THEM, WE WILL PUT AN "S" INTO RECORD ITOC(2,I) OF THE
!     LCODE FILE AND PUT A 1 INTO EACH RECORD IN THE LCODE FILE WHICH
!     IS TO BE DUMPED
!
!     BRUCE SCHUPLER  -  8 OCTOBER 1979
!
!# LAST COMPC'ED  830208:18:36                                 #
!
      DIMENSION IBUF(40),ITOC(2,*),IDCBLC_un(16),LCODE(4)
      integer*2 itmptoc
      Character*1 lresp
!
! 1.    CHECK WITH THE USER
!
100   CONTINUE
      write(ilu_stdout,110) IREC
110   FORMAT("Do you want to dump all of record type ",I3," or just ", &
     &       "specific items (A/S)? ",$)
      read(ilu_stdin,120) LRESP
120   FORMAT(A1)
      CALL CASEFOLD(lresp )
      IF((LRESP.NE.'A') .AND. (LRESP.NE.'S')) GO TO 100
      IF(LRESP.EQ.'A') GO TO 400
!
! 2.   WELL, WE ARE ONLY DUMPING SOME ITEMS.  MARK THE TOC
!
      DO 200 I=1,MAXTOC
      IPOINT = I
      IF(ITOC(1,I) .NE. IREC) GO TO 200
      GO TO 210
200   CONTINUE
210   CONTINUE
!      CALL READF(IDCBLC_un(1),IERR,IBUF,40,LEN,ITOC(2,IPOINT))
      READ(unit=IDCBLC_un(1),iostat=IOS,rec=ITOC(2, &
     &                                                  IPOINT))(IBUF(i),i=1,40)
      IERR = IOS
      IBUF(12) = 'S'
!     CALL WRITF(IDCBLC_un(1),IERR,IBUF,40,ITOC(2,IPOINT))
      WRITe(unit=IDCBLC_un(1),iostat=IOS,rec=ITOC(2, &
     &                                          IPOINT))(IBUF(i),i=1,40)
      IERR = IOS
!
! 3.    SEE WHICH LCODES WE ARE DUMPING AND MARK THEM AS SUCH
!
      IMANY = ITOC(2,IPOINT+1) - ITOC(2,IPOINT) - 1
      IF((IPOINT .EQ. MAXTOC) .OR. (ITOC(2,IPOINT+1) .EQ. &
     &      0))IMANY = ILAST - ITOC(2,IPOINT) - 1
300   CONTINUE
      CALL BLANK( LCODE, INT2(4) )
      write(ilu_stdout,310)
310   FORMAT("What code do you want to dump (:: to end list)? ",$)
      read(ilu_stdin,320) LCODE
320   FORMAT(4A2)
      IF(LCODE(1) .EQ. 2H::) GO TO 400
!
! 3.1   SEARCH FOR THE LCODE
!
!      CALL READF(IDCBLC_un(1),IERR,IBUF,40,LEN,ITOC(2,IPOINT))
      itmptoc=itoc(2,ipoint)
      READ(unit=IDCBLC_un(1),iostat=IOS,rec=itmptoc)(IBUF(i),i=1,40)
      IERR = IOS
      DO 340 K=1,IMANY
!     CALL READF(IDCBLC_un(1),IERR,IBUF,40,LEN)
      itmptoc = itmptoc+1
      READ(unit=IDCBLC_un(1),iostat=IOS,rec=itmptoc)(IBUF(i),i=1,40)
      IERR = IOS
        DO 330 J=1,4
        IF(LCODE(J) .NE. IBUF(1+J)) GO TO 340
330     CONTINUE
        IBUF(31) = '1'
!        CALL WRITF(IDCBLC_un(1),IERR,IBUF,40,ITOC(2,IPOINT) + K)
!        itmptoc = ITOC(2,IPOINT) + K  !made unnecessary by other changes
        WRITe(unit=IDCBLC_un(1),iostat=IOS, &
     &    rec=itmptoc)(IBUF(i),i=1,40)
        IERR = IOS
        GO TO 300
340   CONTINUE
      write(ilu_stdout,350) LCODE
350   FORMAT("Code ",4A2," not found!")
      GO TO 300
!
! 4.    THE END
!
400   CONTINUE
      RETURN
      END
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BLANK(IBUF,NWORDS)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  IOS
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 ibuf, nwords, lblnk, i
!-----END of imp added lines.
!
!                                  ,C#830208:18:36#
!
! 19.    BLANK
!
! 19.1   BLANK PROGRAM SPECIFICATION
!
! 19.1.1 BLANK WILL FILL THE FIRST NWORDS OF IBUF WITH BLANKS
!
! 19.1.2 RESTRICTIONS - NONE
!
! 19.1.3 REFERENCES - NONE
!
! 19.2   BLANK PROGRAM INTERFACE
!
! 19.2.2 CALLING SEQUENCE - CALL BLANK(IBUF,NWORDS)
!
!       INPUT VARIABLES -
!
!       1. IBUF(1) - THE ARRAY TO BE BLANK FILLED
!
!       2. NWORDS - THE NUMBER OF WORDS OF IBUF TO BE BLANK FILLED
!
!       OUTPUT VARIABLES
!
!       1. IBUF(1) - THE BUFFER AFTER BLANK FILLING
!
! 19.2.2 COMMON BLOCKS USED - NONE
!
! 19.2.3 PROGRAM SPECIFICATIONS
!
      DIMENSION IBUF(*)
!
      DATA LBLNK /2H  /
!
! 19.2.4 DATA BASE ACCESS - NONE
!
! 19.2.5 EXTERNAL INPUT/OUTPUT - NONE
!
! 19.2.6 SUBROUTINE INTERFACE
!
!       CALLER SUBROUTINES - USER PROGRAM
!
!       CALLED SUBROUTINES - NONE
!
! 19.2.7 CONSTANTS USED - NONE
!
! 19.2.8 PROGRAM VARIABLES -
!
!        1) LBLNK - LITERALLY "  "
!
! 19.2.9 PROGRAMMER - BRUCE SCHUPLER 15 MARCH 1978
!
! 19.3   BLANK PROGRAM STRUCTURE
!
!  1.    BLANK FILL NWORDS OF IBUF
!
      DO 100 I=1,NWORDS
      IBUF(I) = LBLNK
  100 CONTINUE
!
! 2. THE END
!
      RETURN
      END
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ERRUS()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  IOS
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 idcblc_un, iscrcr
!-----END of imp added lines.
!
!                     ,C#830208:18:36#
!
! 1.    INTERCEPTS READ ONLY DATA BASE HANDLER ERRORS
!
      COMMON /DBDMP/ IDCBLC_un(16),LNAME_CHR,ISCRCR
      CHARACTER*12 LNAME_CHR
!
!      CALL CLOSE(IDCBLC_un(1))
      CLOSE(unit=IDCBLC_un(1),status='DELETE')
!      CALL EXEC(6)
      STOP
      END
