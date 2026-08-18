      Subroutine make_cfile (cfu, cfname, dfname, ndata, idate, mm, &
     &  rate, sigr, ily, ilm, ild, ify, ifm, ifd, change_span, &
     &  spool_name, rchi, wrms)
!
!     Plot up, north, east information
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!
!     Updated to specificaly type integers which
!-------------------------------------------------
      integer*2 cfu, ndata, len, trimlen
      integer*2 ilast(3)
      integer*2 num_sess_type,i
      Integer*2 ios, idate(3,*), Ierr
      Integer*2 ify, ily, ifm, ilm, ifd, ild
      Integer*2 mm(2,3)
      Integer*4 Ierr4
!
      real*8 rate(3), sigr(3), rchi(3), wrms(3)
      real*8 wtd_mean_U, wtd_mean_E, wtd_mean_N
      real*8 sig_mean_U, sig_mean_E, sig_mean_N
      real*8 time_span, fjldy
!
      Real*8 MinRU,  MinRE, MinRN
      Real*8 Urats,wrms_U, Erat,Erats,wrms_E, Nrat,Nrats,wrms_N
      Real*8 Red_chi_U, Red_chi_E, Red_chi_N
      Real*8 mm_real(2)
!
      Character    CTIME*12,CDATE*16
      Character*50 cfname
      Character*80 dfname
      Character*157 spool_name
!
      Logical*2 error_bars, quit, do_specify_types
      LOGICAL*2 plot_on_screen
      LOGICAL*2 plot_total
      LOGICAL*2 change_span
      logical*4   kopen
!
!
!
!     Modification history:
!
!     kdb 001026 Fix error.  Leftover, improperly commented out code was
!                printing an uninitialized date and time on the bottom
!                left corner of the plots.  (This was previously undetected
!                because it was randomly set to blanks.)  Stop printing this.
!     kdb 060426 Fix a problem found by the linux/ftn90 compiler (an attempt
!                by three write statements to write two integer variables 
!                (mm(n,m)) using floating formats).
!
!     plot_line = .true.
      if (.not. change_span) then
        ify = idate(1,1)
        ily = idate(1,ndata)
        ifm = idate(2,1)
        ilm = idate(2,ndata)
        ifd = idate(3,1)
        ild = idate(3,ndata)
      end if
!
!
!
      time_span = &
     &  fjldy(ilm, ild, ily) - &
     &  fjldy(ifm, ifd, ify)
!     If(time_span.lt.180.d0) plot_line = .false.
!
!     Convert rate and sigma rate from mm/day to mm/year
!
      do i = 1, 3
        rate(i) = rate(i)*365.25d0
        sigr(i) = sigr(i)*365.25d0
      end do
!
!
!
!       Build up the control file for Frank Gomez's pgplot (fgplot).
!
!       inquire(cfu,opened=kopen)
        Write(cfu,'("#   ploting uen information              ")')
        Write(cfu,'("#                                        ")')
        Write(cfu,'("#   section for up                       ")')
        Write(cfu,'("#                                        ")')
        Write(cfu,'("begin")')
        Write(cfu,'("headers 3")')
        Write(cfu,'("line 0")')
!
!       If(plot_total) then
!         If(error_bars) then
            Write(cfu,'("y_field 1 5 6 Up (mm)")')
!         else
!           Write(cfu,'("y_field 1 5 0 Up (mm)")')
!         Endif
!       else !plot residuals
!         If(error_bars) then
!           Write(cfu,'("y_field 1 8 6 Residual Up (mm)")')
!         else
!           Write(cfu,'("y_field 1 8 0 Residual Up (mm)")')
!         Endif
!       endif
!
        Write(cfu,'("x_field 0 2 4")')
        Write(cfu,'("view .1 .9 .68 .91")')
!       If(plot_total) then
          mm_real(1) = dble(mm(1,1))
          mm_real(2) = dble(mm(2,1))
          Write(cfu, &
     &    '("window ",6I3,1x,f10.1,1x,f10.1)')ify, ifm, ifd, ily, ilm, ild, &
     &    mm_real(2), mm_real(1)
!       else
!         Write(cfu,'("window ",6I3,1x,f10.1,1x,f10.1)')
!     .    ify, ifm, ifd, ily, ilm, ild,
!    .    MinRU, MaxRU
!       endif
        Write(cfu,'("ebz 0.0")')
        Write(cfu,'("axes ABMCT ABCNT")')
        len = trimlen(dfname)
        Write(cfu,'("file ",a)') dfname(1:len)
        Write(cfu,'("read")')
        Write(cfu,'("charsz 0.6")')
        Write(cfu,'("label .35 .97 1 0  \h1")')
!       Write(cfu,'("label .20 .95 1 0  \h2")')
!
!        Write(cfu,'("label .70 .93 1 0  Number of Sessions =",i5)')
!      .  n_sess
!        Write(cfu,'("label .15 .93 1 0  Boxcar window",i4," days")')
!     .  boxcar_window
!
!       If(plot_total) then
          Write(cfu,'("label .10 .66 1 0  ", &
     &    " Rate =",f5.1," +/-",f5.1," mm/yr", &
     &    "  Wrms of fit =",f5.1," mm", &
     &    "  Reduced Chi-square =",f6.2)') &
     &    rate(1), sigr(1), wrms(1), rchi(1)
!
        Write(cfu,'("point 3")')
        Write(cfu,'("draw")')
!        If(plot_line .and. plot_total) then
           Write(cfu,'("y_field 1 7 0")')
           Write(cfu,'("line 1")')
           Write(cfu,'("point 1")')
           Write(cfu,'("read")')
!          Write(cfu,'("axes ABCT ABCNT")')
           Write(cfu,'("draw")')
!
!
        Write(cfu,'("#                                        ")')
        Write(cfu,'("#   section for east                     ")')
        Write(cfu,'("#                                        ")')
        Write(cfu,'("line 0")')
        Write(cfu,'("y_field 1 9 10 East (mm)")')
        Write(cfu,'("x_field 0 2 4")')
        Write(cfu,'("view .1 .9 .38 .61")')
!
          mm_real(1) = dble(mm(1,2))
          mm_real(2) = dble(mm(2,2))
          Write(cfu, &
     &    '("window ",6I3,1x,f10.1,1x,f10.1)')ify, ifm, ifd, ily, ilm, ild, &
     &    mm_real(2), mm_real(1)
!
        Write(cfu,'("ebz 0.0")')
        Write(cfu,'("axes ABCMT ABCNT")')
        Write(cfu,'("read")')
!       if(plot_total) then
          Write(cfu,'("label .10 .36 1 0  ", &
     &    "  Rate =",f5.1," +/-",f5.1," mm/yr", &
     &    "  Wrms of fit =",f5.1," mm", &
     &    "  Reduced Chi-square =",f6.2)') &
     &    rate(2), sigr(2), wrms(2), rchi(2)
!
        Write(cfu,'("point 3")')
        Write(cfu,'("draw")')
!         if(plot_line.and.plot_total) then
           Write(cfu,'("y_field 1 11 0")')
           Write(cfu,'("line 1")')
           Write(cfu,'("point 1")')
           Write(cfu,'("read")')
!          Write(cfu,'("axes ABCT ABCNT")')
           Write(cfu,'("draw")')
!
!
        Write(cfu,'("#                                        ")')
        Write(cfu,'("#   section for North                    ")')
        Write(cfu,'("#                                        ")')
        Write(cfu,'("line 0")')
!
!       If(plot_total) then
!         If(error_bars) then
            Write(cfu,'("y_field 1 13 14 North(mm)")')
!         else
!           Write(cfu,'("y_field 1 13  0 North(mm)")')
!         Endif
!       else
!         If(error_bars) then
!           Write(cfu,'("y_field 1 16 14 Residual North(mm)")')
!         else
!           Write(cfu,'("y_field 1 16  0 Residual North(mm)")')
!         Endif
!       endif
!
        Write(cfu,'("x_field 0 2 4")')
        Write(cfu,'("view .1 .9 .07 .30")')
!       If(plot_total) then
          mm_real(1) = dble(mm(1,3))
          mm_real(2) = dble(mm(2,3))
          Write(cfu, &
     &    '("window ",6I3,1x,f10.1,1x,f10.1)')ify, ifm, ifd, ily, ilm, ild, &
     &    mm_real(2), mm_real(1)
!       else
!         Write(cfu,'("window ",6I3,1x,f10.1,1x,f10.1)')
!    .    ify, ifm, ifd, ily, ilm, ild
!    .    MinRN, MaxRN
!       endif
!
        Write(cfu,'("ebz 0.0")')
        Write(cfu,'("axes ABCMT ABCNT")')
        Write(cfu,'("read")')
!       if(plot_total) then
          Write(cfu,'("label .1 .05 1 0 ", &
     &    " Rate =",f5.1," +/-",f5.1," mm/yr", &
     &    "  Wrms of fit =",f5.1," mm", &
     &    "  Reduced Chi-square =",f6.2)') &
     &    rate(3), sigr(3), wrms(3), rchi(3)
!          Write(cfu,'("label .1 .05 1 )
!     .    f5.1," +/-",f5.1," mm/yr",
!     .    " Wrms of fit =",f5.1," mm  Reduced Chi-square =",f6.2)')
!     .    Nrat*365.25,Nrats*365.25,wrms_N,Red_chi_N
!          Call ztime ( cdate, 2 )
!          CALL ztime ( ctime, 1 )
!c
!          write(cfu,'("label .10 .035 1 0  Wtd. mean =",f12.1," +/-",
!     .    f6.1," mm (unscaled)        wrms_about_mean = ",f7.1," mm")')
!     .    wtd_mean_N, sig_mean_N, wrms_about_mean_N
!c
!         write(cfu,'("label .10 .02  1 0  ",a16,1x,a12)') cdate,ctime
!       endif
!
        len = trimlen(spool_name)
        Write(cfu, &
     &  '("label .45 .02  1 0 Spool file = ",a)')spool_name(1:len)
!        If(do_specify_types) then
!          write(cfu,'("label .10 .00 1 0 Session selection: ",
!     .    5(a12,":"))') (qsess_type(i),i=1,num_sess_type)
!        else
!         write(cfu,'("label .10 .00 1 0 Session selection:",
!    .    " Unrestricted.")')
!        endif
        Write(cfu,'("point 3")')
        Write(cfu,'("draw")')
!        if(plot_line .and. plot_total) then
           Write(cfu,'("y_field 1 15 0")')
           Write(cfu,'("line 1")')
           Write(cfu,'("point 1")')
           Write(cfu,'("read")')
!          Write(cfu,'("axes ABCT ABCNT")')
           Write(cfu,'("draw")')
!
!         Plot boxcar in total space
!          Write(cfu,'("y_field 1 23 0")')
!          Write(cfu,'("line 4")')
!          Write(cfu,'("point 1")')
!          Write(cfu,'("read")')
!          Write(cfu,'("draw")')
!        endif
!
!       Plot the boxcar line residual space
!        If(.not.plot_total) then
!          write(cfu,'("y_field 1 20 0")')
!          write(cfu,'("line 1")')
!           write(cfu,'("point 1")')
!           Write(cfu,'("ebz 0.0")')
!           write(cfu,'("read")')
!           Write(cfu,'("draw")')
!        endif
!
        Write(cfu,'("end")')
        Write(cfu,'("#                                        ")')
        Close (cfu)
!
!
!
      return
      End
