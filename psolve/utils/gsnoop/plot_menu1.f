      SUBROUTINE plot_menu1 &
     &      (kterm, qstat, qdev, mfname, cfname, dfname, &
     &       mlu, clu, dlu, mm, ily, ilm, ild, ify, ifm, ifd, &
     &       jd1, jd2, change_span, qcpl_stat, ncplstat)
!
!
!     purpose: display, handle UEN plotting menu
!
!     modifications
!
!     kdb 000831 Fix bug.  Previously commands u, d and l would leave the
!                screen blank after executing, implying that the program was
!                stalled.  Now refresh the screen after executing u, d and l.
!     kdb 060323 Senkr_mn handles its input via an integer*4 variable, which
!                is subject to linux endian conversion problems.  So 
!                this subroutine will now transfer senkr_mn's input to a 
!                character variable and work with that.
!
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
      integer*2    i, nstat, ncplstat
      INTEGER*4    ios
      integer*2    mm(2,3)
!     Updated to specificaly type integers which
!-------------------------------------------------
      integer*2    mlu, clu, dlu
      integer*2    ilm, ily, ild, ifm, ify, ifd
      integer*2    l1, l2, l3, trimlen
      integer*2    ilfy1, ilfm1, ilfd1, ilfy2, ilfm2, ilfd2
      integer*4    ix, iy, ich
      character*4 cch
      equivalence (ich, cch)
!
!
      LOGICAL*2 kdone, kterm, krefresh
      LOGICAL*2 change_span
      LOGICAL*2 knewmf
!
      real*8       jd1, jd2, fjldy
!
      character*8  qstat, qcpl_stat(*)
      character*20 qlf    !linear fit date range statement
      character*10 qdev
      character*50 cfname
      character*80 dfname, mfname, qstr
!
!
!
      qstat = qcpl_stat(1)
      nstat = 1
      knewmf = .false.
      kdone  = .false.
      krefresh = .true.
      qlf  = "full span"
      qdev = "/XWINDOWS "
!
      if (change_span .eq. .false.) then
        ilm  = 00
        ily  = 0
        ild  = 0
        ifm  = 0
        ify  = 0
        ifd  = 0
      end if
!
!
!
 110  do while (.not. kdone)
        ix = 0
        iy = 0
        call setcr_mn(ix, iy )
        write (qstr,'(79x)')
        do i = 1, 25
          call asnl(qstr )
        end do
!
          l1 = trimlen(mfname)
          if (l1.gt.60) l1=60
          l2 = trimlen(qstat)
          l3 = trimlen(qdev)
!
        if (krefresh) then
          krefresh = .false.
          call setcr_mn(ix, iy )
          call clrtobot_mn()
          call refresh_mn()
!
        qstr="UEN plotting menu                                   94/09"
        call as2nl(qstr )
!
        write (qstr,"('(u) UEN file name: ',a)") mfname(1:l1)
        call as2nl(qstr )
!
        write (qstr,"('(s) Station name: ',a)") qstat(1:l2)
        call as2nl(qstr )
!
        write (qstr,"('(d) Device for output: ',a)") qdev(1:l3)
        call as2nl(qstr )
!
        write (qstr,"('(l) Date range for linear fit: 'a)") qlf
        call as2nl(qstr )
!
        qstr= "(c) Change scales"
        call as2nl(qstr )
!
        write (qstr,"('(r) Refresh screen: ')")
        call as2nl(qstr )
!
        qstr="(h) Hard copy "
        call as2nl(qstr )
!
        qstr="(p) Plot                            (t) Terminate program"
        call as2nl(qstr )
!
        call refresh_mn()
        end if
!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
!
!
!       sense response and set flags
!
        call senkr_mn (ix, iy, ich )
        call casefold (cch)
!
!
!
!       if (ich .eq. 67) then   !c
        if (cch(4:4) .eq. 'C') then   !c
!
          call &
     &            plot_scale(mm, ily, ilm, ild, ify, ifm, ifd, change_span )
          krefresh = .true.
!
!       else if (ich .eq. 68) then
        else if (cch(4:4) .eq. 'D') then   
!
          call sel_dev (qdev )
          krefresh = .true.
!
!
!       else if (ich .eq. 82) then
        else if (cch(4:4) .eq. 'R') then   
          krefresh = .true.
!
!
!       else if (ich .eq. 76) then
        else if (cch(4:4) .eq. 'L') then   
!
          qstr = "Enter first and last date in span."
          call asnl(qstr )
          qstr = 'yy mm dd yy mm dd'
          call asnl(qstr )
!
          call getstr_f(qstr )
          read (qstr , *, &
     &      iostat=ios)ilfy1, ilfm1, ilfd1, ilfy2, ilfm2, ilfd2
          write (qlf,"(2(x,(3(x,i2))))", &
     &      iostat=ios)ilfy1, ilfm1, ilfd1, ilfy2, ilfm2, ilfd2
          jd1 = fjldy(ilfm1, ilfd1, ilfy1)
          jd2 = fjldy(ilfm2, ilfd2, ilfy2)
          krefresh = .true.
!
!
!       else if (ich .eq. 72) then
        else if (cch(4:4) .eq. 'H') then
!
          call &
     &         open_plot_files(mfname, cfname, dfname, qstat, mlu, dlu, clu, knewmf )
          qdev = "/HPGLP    "
          kdone= .true.
          krefresh = .true.
!
!       else if (ich .eq. 80) then
        else if (cch(4:4) .eq. 'P') then
!
          call &
     &         open_plot_files(mfname, cfname, dfname, qstat, mlu, dlu, clu, knewmf )
          qdev = "/XWINDOWS "
          kdone = .true.
          krefresh = .true.
!
!       else if (ich .eq. 83) then
        else if (cch(4:4) .eq. 'S') then
!
          nstat = nstat + 1
          if (nstat .gt. ncplstat) nstat = 1
          qstat = qcpl_stat (nstat)
          call setcr_mn (18, 5 )
          call addstr_f (qstat )
          krefresh = .true.
!
!       else if (ich .eq. 85) then
        else if (cch(4:4) .eq. 'U') then
!
          write (qstr, "('Enter CPL UEN file name: ',$)")
          call asnl(qstr )
          call getstr_f(mfname )
!
          call setcr_mn (19, 3 )
          call addstr_f (mfname )
          knewmf = .true.
          krefresh = .true.
!
!
!       else if (ich .eq. 84) then
        else if (cch(4:4) .eq. 'T') then
            krefresh = .true.
          write (qstr,'("Sure you want to terminate now? (y/n)")')
          call asnl(qstr )
          call senkr_mn (ix, iy, ich )
          call casefold(cch)
!         if (ich .eq. 89) then
          if (cch(4:4) .eq. 'Y') then
            kterm = .true.
            kdone = .true.
          end if
!
!
        else
          continue
!
!
        end if
!
!
      end do
!
!
!
!
      return
      end
