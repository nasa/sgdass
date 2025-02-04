      SUBROUTINE sel_dev (qdev)
!
!     Output device selecion menu
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!     060323 kdb Senkr_mn handles its input via an integer*4 variable, which
!                is subject to linux endian conversion problems.  So 
!                this subroutine will now transfer senkr_mn's input to a 
!                character variable and work with that.
!
!
      integer*4    ix, iy, ich
      character*4 cch
      equivalence (ich, cch)
!
!
      LOGICAL*2 kdone
!
      character*10 qdev
      character*10 qd(12)
      character*80 qstr
!
!
!
!
!
      kdone  = .false.
      qd(1)  = "/LJHIGHRES"
      qd(2)  = "/LJHIGLAND"
      qd(3)  = "/LJLOWLAND"
      qd(4)  = "/XWINDOWS "
      qd(5)  = "/LJLOWRES "
      qd(6)  = "/TEKLAND  "
      qd(7)  = "/TEK4010  "
      qd(8)  = "/HPGLL    "
      qd(9)  = "/HPGLP    "
      qd(10) = "/NULL     "
      qd(11) = "/PS       "
      qd(12) = "/X        "
!
!
!
!
 110  do while (.not. kdone)
        ix = 0
        iy = 0
        call setcr_mn (ix, iy )
        call clrtobot_mn()
        call refresh_mn()
!
!
!
!
       qstr =  "Select output device:     "
       call as2nl(qstr )
       qstr = "(1) = \LJHIGHRES"
       call as2nl(qstr )
       qstr = "(2) = \LJHIGLAND"
       call as2nl(qstr )
       qstr = "(3) = \LJLOWLAND"
       call as2nl(qstr )
       qstr = "(4) = \XWINDOWS "
       call as2nl(qstr )
       qstr = "(5) = \LJLOWRES "
       call as2nl(qstr )
       qstr = "(6) = \TEKLAND  "
       call as2nl(qstr )
       qstr = "(7) = \TEK4010  "
       call as2nl(qstr )
       qstr = "(8) = \HPGLL    "
       call as2nl(qstr )
       qstr = "(9) = \HPGLP    "
       call as2nl(qstr )
       qstr = "(a) = \NULL     "
       call as2nl(qstr )
       qstr = "(b) = \PS       "
       call as2nl(qstr )
!
!
        call senkr_mn(ix, iy, ich )
        call casefold (cch)
!
!       sense response and set flags
!
!
!       if (ich .eq. 49) then
        if (cch(4:4) .eq. '1') then
          qdev = qd(1)
!
!       else if (ich .eq. 50) then
        else if (cch(4:4) .eq. '2') then
          qdev = qd(2)
!
!       else if (ich .eq. 51) then
        else if (cch(4:4) .eq. '3') then
          qdev = qd(3)
!
!       else if (ich .eq. 52) then
        else if (cch(4:4) .eq. '4') then
          qdev = qd(4)
!
!       else if (ich .eq. 53) then
        else if (cch(4:4) .eq. '5') then
          qdev = qd(5)
!
!       else if (ich .eq. 54) then
        else if (cch(4:4) .eq. '6') then
          qdev = qd(6)
!
!       else if (ich .eq. 55) then
        else if (cch(4:4) .eq. '7') then
          qdev = qd(7)
!
!       else if (ich .eq. 56) then
        else if (cch(4:4) .eq. '8') then
          qdev = qd(8)
!
!       else if (ich .eq. 57) then
        else if (cch(4:4) .eq. '9') then
          qdev = qd(9)
!
!       else if (ich .eq. 65) then
        else if (cch(4:4) .eq. 'A') then
          qdev = qd(10)
!
!       else if (ich .eq. 66) then
        else if (cch(4:4) .eq. 'B') then
          qdev = qd(11)
!
!       else if (ich .eq. 82) then
        else if (cch(4:4) .eq. 'R') then
          kdone = .true.
!
        else
          continue
!
        end if
!
        kdone = .true.
!
      end do
!
!
!
!
      return
      end
