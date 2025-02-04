      SUBROUTINE plot_scale (mm, ily, ilm, ild, ify, ifm, ifd, &
     &                       change_span)
!
!     Output device selecion menu
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     060323 kdb Senkr_mn handles its input via an integer*4 variable, which
!                is subject to linux endian conversion problems.  So 
!                this subroutine will now transfer senkr_mn's input to a 
!                character variable and work with that.
!
!     Updated to specificaly type integers which
!-------------------------------------------------
!
      integer*2    i, j
      INTEGER*4    IOS
      integer*2    mm(2,3)
      integer*2    ilm, ily, ild, ifm, ify, ifd
!
      integer*4    ix, iy, ich
      character*4 cch
      equivalence (ich, cch)
!
!
      LOGICAL*2 kdone
      LOGICAL*2 change_span
!
      character*80 qstr
!
!
      ios = 0
      kdone  = .false.
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
        call setcr_mn (ix, iy )
        call clrtobot_mn()
        call refresh_mn()
!
!
!
!
 1010  qstr =  "Change scales: "
       call as2nl(qstr )
!
       write (qstr, '("(1) Maximum Up    ",i4)') mm(1,1)
       call as2nl(qstr )
!
       write (qstr, '("(2) Minimum Up    ",i4)') mm(2,1)
       call as2nl(qstr )
!
       write (qstr, '("(3) Maximum East  ",i4)') mm(1,2)
       call as2nl(qstr )
!
       write (qstr, '("(4) Minimun East  ",i4)') mm(2,2)
       call as2nl(qstr )
!
       write (qstr, '("(5) Maximum North ",i4)') mm(1,3)
       call as2nl(qstr )
!
       write (qstr, '("(6) Minimun North ",i4)') mm(2,3)
       call as2nl(qstr )
!
       qstr = "(a) All six "
       call as2nl(qstr )
!
      if (change_span) then
         write (qstr , &
     &         '("(s) Time span: ",2(x,3(i3)))')ify, ifm, ifd, ily, ilm, ild
      else
         write (qstr , '("(s) Time span: full span")')
      end if
      call as2nl(qstr )
!
       qstr = "(r) RETURN"
       call as2nl(qstr )
!
!
!
!
!       sense response and set flags
!
        call senkr_mn(ix, iy, ich )
        call casefold (cch)
!       if  ((ich .eq. 49).or.(ich .eq. 50).or.(ich .eq. &
!    &   51).or.(ich .eq. 52).or.(ich .eq. 53).or.(ich .eq. &
!    &   54))qstr = "New Value = "
        if  ((cch(4:4) .eq. '1').or.(cch(4:4) .eq. '2').or.(cch(4:4) .eq. &
     &   '3').or.(cch(4:4) .eq. '4').or.(cch(4:4) .eq. '5').or.(cch(4:4) .eq. &
     &   '6'))qstr = "New Value = "
!
!       if (ich .eq. 49) then
        if (cch(4:4) .eq. '1') then
!
          call asnl(qstr )
          call getstr_f(qstr )
          read (qstr,      *, iostat=ios, err=911) mm(1,1)
!
!       else if (ich .eq. 50) then
        else if (cch(4:4) .eq. '2') then
!
          call asnl(qstr )
          call getstr_f(qstr )
          read (qstr,      *, iostat=ios, err=911) mm(2,1)
!
!       else if (ich .eq. 51) then
        else if (cch(4:4) .eq. '3') then
!
          call asnl(qstr )
          call getstr_f(qstr )
          read (qstr,      *, iostat=ios, err=911) mm(1,2)
!
!       else if (ich .eq. 52) then
        else if (cch(4:4) .eq. '4') then
!
          call asnl(qstr )
          call getstr_f(qstr )
          read (qstr,      *, iostat=ios, err=911) mm(2,2)
!
!       else if (ich .eq. 53) then
        else if (cch(4:4) .eq. '5') then
!
          call asnl(qstr )
          call getstr_f(qstr )
          read (qstr,      *, iostat=ios, err=911) mm(1,3)
!
!       else if (ich .eq. 54) then
        else if (cch(4:4) .eq. '6') then
!
          call asnl(qstr )
          call getstr_f(qstr )
          read (qstr,      *, iostat=ios, err=911) mm(2,3)
!
!       else if (ich .eq. 65) then
        else if (cch(4:4) .eq. 'A') then
!
          qstr =  &
     &            "Enter new values, free format, in same order as list"//"ed above."
          call asnl(qstr )
!
          call getstr_f(qstr )
          read (qstr, *, iostat=ios, err=911) ((mm(i,j),i=1,2),j=1,3)
!
          if (ios .ne. 0) then
             write (qstr, "('Input error ',i4)") ios
             call asnl(qstr )
             qstr= "<RETURN> to continue"
             call asnl(qstr )
             call getstr_f(qstr )
          end if
!
!       else if (ich .eq. 83) then
        else if (cch(4:4) .eq. 'S') then
!
          qstr = "Enter first and last date in span."
          call asnl(qstr )
          qstr = 'yy mm dd yy mm dd'
          call asnl(qstr )
!
          call getstr_f(qstr )
          read (qstr , *, iostat=ios, &
     &            err=911)ify, ifm, ifd, ily, ilm, ild
          change_span = .true.
!
!       else if (ich .eq. 82) then
        else if (cch(4:4) .eq. 'R') then
!
          kdone = .true.
!
        else
          continue
!
        end if
!
911     if (ios .ne. 0) then
          write (qstr, '("Error ",i5," <RETURN> to continue.")')
          call asnl(qstr )
          call getstr_f(qstr )
        end if
!
      end do
!
!
!
!
      return
      end
