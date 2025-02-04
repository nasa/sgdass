      SUBROUTINE plot_uen(spool_name, mfname, ncoldat, qcpl_stat, &
     &nstat )
!
!     PROGRAM TO plot uen for stations from gsnoop
!
!     kdb 990127 Fix heap allocation request error.  (Fplot has acquired a
!                third argument.)
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     Updated to specificaly type integers which
!-------------------------------------------------
      integer*2       idate(3,1000), ndata
      integer*2       ncoldat, ncol, nstat
      parameter       (ncol=20)
      integer*2       mlu, clu, dlu
      integer*2       i, ierr
      integer*2       mm(2,3)            !max, min, u,e,n D
      integer*2       ilm, ily, ild, ifm, ify, ifd
      integer*4       fplot, ier4
!
      real*8          jd1, jd2
      real*8          rate(3), sigr(3)
      real*8          ydata(ncol,1000)
      real*8          lrchi(3), wrms(3)
!
      LOGICAL*2 kdone, kterm
      LOGICAL*2 change_span
!
      character*8     qstat
      character*8     qcpl_stat(*)
      character*10    qdev
      character*50    cfname
      character*80    dfname, mfname, qhead(2)
      character*157   spool_name
!
!     common          cfname, ijunk, qdev, ijunk2
!
      mlu = 0
      dlu = 0
      clu = 0
      jd1 = 0
      jd2 = 1.0d9
      qdev = "/XWINDOWS "
      kdone = .false.
      kterm = .false.
      change_span = .false.
!
      do i = 1,3
       mm(1,i) = 100
       mm(2,i) = -100
      end do
!
!      write (qstat(1:),'("None yet")')
!       qstat = "HRAS 085"
!
!
! Control
      do while (.not. kterm)
        call &
     &       plot_menu1(kterm, qstat, qdev, mfname, cfname, dfname,  mlu, clu, dlu, &
     &       mm, ily, ilm, ild, ify, ifm, ifd, jd1, jd2, change_span, &
     &       qcpl_stat, nstat )
       if (.not. kterm) then
          call read_data (ndata, ncoldat, idate, ydata, qstat,qhead, &
     &                     mlu, jd1, jd2 )
          if (ndata.gt. &
     &    2)call fit_line (ncol, ndata, idate, ydata, rate, sigr, lrchi, &
     &                   wrms)
          if (ndata.gt. &
     &    2)call make_dfile(ncol, ndata, idate, ydata, qhead, dlu)
          if (ndata.gt. &
     &    2)call make_cfile(clu, cfname, dfname, ndata, idate, mm, &
     &      rate, sigr, ily, ilm, ild, ify, ifm, ifd, change_span, &
     &      spool_name, lrchi, wrms)
          if (ndata.gt. &
     &     2)ier4 = fplot(cfname, qdev,'')
        end if ! (.not. kterm
        ierr = 0
      end do ! while (.not. kterm)
!
!
!
      end
