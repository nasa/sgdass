      SUBROUTINE open_plot_files &
     &  (mfname, cfname, dfname, qstat, mlu, dlu, clu, knewmf)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      integer*2      i, lt, len, ltn, trimlen
      integer*4      ios
!     Updated to specificaly type integers which
!-------------------------------------------------
      integer*2      getunit, mlu, clu, dlu
!
!
      Character*8    qstat
      Character*12   CTIME
      Character*50   cfname
      Character*80   dfname, mfname, qstr
!
      LOGICAL*2 knewmf
      logical*4      kopen
!
!
!
      clu = 51
      dlu = 52
      mlu = 53
!
      CALL ztime ( ctime, INT2(1) )
      lt = trimlen(ctime)
      len = trimlen(qstat)
!
!     Open the plot control file.
      write (cfname(1:), &
     &      '(a9,a,a8)')'/tmp/plt_',qstat(1:len),ctime(1:lt-3)
      ltn = trimlen(cfname)
      do i = 1, ltn
        if ((cfname(i:i) .eq. " ").or.(cfname(i:i) .eq. &
     &          "."))cfname(i:i) = "_"
      end do
      if (clu .eq. 0) clu = getunit()
      inquire(clu,opened=kopen)
      if (kopen) close (clu)
      OPEN(clu,Iostat=ios,Err=900,File=cfname(1:ltn),Status='unknown')
 900  Write(qstr,'("IOS from open of plot control file is",I5)') Ios
      call asnl(qstr )
!
!
!     Open the plot data file.  Use the first four characters of the
!     station name in lower case, and the time.
      if(dlu .eq. 0) dlu = getunit()
      write (dfname(1:), &
     &       '(a9,a,a8)')'/tmp/uen_',qstat(1:len),ctime(1:lt-3)
      ltn = trimlen(dfname)
      do i = 1, ltn
        if ((dfname(i:i) .eq. " ").or.(dfname(i:i) .eq. &
     &          "."))dfname(i:i) = "_"
      end do
      inquire(dlu,opened=kopen)
      if (kopen) close (dlu)
      OPEN(dlu, Iostat=ios,FILE=dfname(1:ltn), Status='unknown')
      Write(qstr,'("IOS from open of plot data file is",I5)') Ios
      call asnl(qstr )
      If(ios.ne.0) then
        write(qstr,'("Failure to open UEN data file. ")')
        call asnl(qstr )
        Write(qstr,'("IOS from OPEN is ",I5)') IOS
        call asnl(qstr )
        stop
      Endif
      Write(dlu,'("  Topocentric plots for ",a8)') qstat
!
!
!      Open the master plot data file.
!
      inquire(mlu,opened=kopen)
      if (kopen) close (mlu)
      OPEN(mlu, Iostat=ios,FILE=mfname, Status='old')
      Write(qstr,'("IOS from open of master data file is",I5)') Ios
      call asnl(qstr )
      knewmf = .false.
      If(ios.ne.0) then
        write(qstr,'("Failure to open master UEN data file.")')
        call asnl(qstr )
        Write(qstr,'("IOS from OPEN is ",I5)') IOS
        call asnl(qstr )
        qstr = "<RETURN> to continue"
        call asnl(qstr )
        call getstr_f(qstr )
      Endif
!
!
!
!
        return
        end
