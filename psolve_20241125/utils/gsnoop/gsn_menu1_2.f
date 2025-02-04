      SUBROUTINE gsn_menu1_2 (kdon2, k1_1)
!
!     Positions and velocities output menu
!
!     modifications
!
!     000905 kdb Make options 3 and 4 obsolete since they only duplicate
!                option 2.
!     000929 kdb Sort the designated continuous piecewise linear sites
!                alphabetically, to guarantee that multiple cpl sites will
!                be sorted alphabetically into the iers_siteid_<> and related
!                files.
!     001026 kdb Fix range error.  In starting list entry, accessed element 0.
!     060323 kdb Senkr_mn handles its input via an integer*4 variable, which
!                is subject to linux endian conversion problems.  So 
!                this subroutine will now transfer senkr_mn's input to a 
!                character variable and work with that.
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
      INCLUDE 'gsnoop_com.i'
!
      integer*2    i
!
      integer*4    ix, iy, ix1, iy1, ix2, iy2, ich
      character*4 cch
      equivalence (ich, cch)
!
!
      LOGICAL*2 kdone, kdon2, krenew, k1_1
!
      character*1  qch
      character*3  qcplstat
      character*80 qstr
      character*8  qcpl_request
!
!
      ix = 0
      iy = 0
      ix1 = 0
      iy1 = 0
      ix2 = 1
      iy2 = 22
      k1_1 = .false.
      kdon2 = .false.
      kdone = .false.
      krenew = .true.
      call setcr_mn (ix1, iy1 )
      call clrtobot_mn()
!
!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
!
 110  do while (.not. kdone)
!
!
       if (krenew) then
         call setcr_mn (ix1, iy1 )
         call clrtobot_mn()
         qstr="GSNOOP positions and velocities menu.              94/07"
         call as2nl(qstr )
!
         write (qstr, &
     &         '("(1) ",l, " Annual XYZ positions table")')do_annual
         call as2nl(qstr )
!
         write (qstr, &
     &         '("(2) ",l, " XYZ & UEN positions & velocities")')do_xyz_vel
         call as2nl(qstr )
!
         write (qstr, '("(3) ",1x, " obsolete     ")')
         call as2nl(qstr )
!
         write (qstr, '("(4) ",1x, " obsolete       ")')
         call as2nl(qstr )
!
         write (qstr, &
     &         '("(5) ",l, " Cmpar files")')do_cmpar
         call as2nl(qstr )
!
         write (qstr, &
     &         '("(6) ",l, " Skip station ",a)')kskip_stat, qskip_stat
         call as2nl(qstr )
!
         write (qstr, &
     &         '("(7) ",l, " Minimum sigma tables ")')do_min_sig
         call as2nl(qstr )
!
         write &
     &    (qstr, '("(s) Select continuous piecewise-linear stations")')
         call as2nl(qstr )
!
         write (qstr, &
!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
     &'("(m) Make tables now.     (r) Refresh     (o) other output menu") &
     &')
         call as2nl(qstr )
!
         write (qstr, &
     &'("(b) Return to main menu                  (t) terminate program") &
     &')
         call as2nl(qstr )
!
         krenew = .false.
       end if
!
!
!       Clear below menu, sense response, and set flags.
!
        call setcr_mn (ix1, iy2 )
        call clrtobot_mn()
        call senkr_mn (ix, iy, ich )
        call casefold (cch)
!
!       Get and act on response
!
!       if (ich .eq. 49) then
        if (cch(4:4) .eq. '1') then
!
          if (do_annual .eq. .false.) then
            do_annual = .true.
            qch = 'T'
          else if (do_annual .eq. .true.) then
            do_annual = .false.
            qch = 'F'
          end if
          call gsn_putchar( INT2(5), INT2(2), qch )
!
!
!       else if (ich .eq. 50) then
        else if (cch(4:4) .eq. '2') then
!
!         Note: over the years options 3 and 4 ended up duplicating option 2,
!               and the variables they set, do_uen_vel and do_uen_adj,
!               became redundant.  Now options 3 and 4 will be disabled,
!               but the internal variables will be kept for now until
!               existing users have a chance to comment on the loss of the
!               options.
!
          if (do_xyz_vel .eq. .false.) then
            do_xyz_vel = .true.
            do_uen_vel = .true.
            do_uen_adj = .true.
            qch = 'T'
          else if (do_xyz_vel .eq. .true.) then
            do_xyz_vel = .false.
            do_uen_vel = .false.
            do_uen_adj = .false.
            qch = 'F'
          end if
          call gsn_putchar( INT2(5), INT2(4), qch )
!
!
!
!       else if (ich .eq. 53) then
        else if (cch(4:4) .eq. '5') then
!
          if (do_cmpar .eq. .false.) then
            do_cmpar   = .true.
            qch = 'T'
          else if (do_cmpar .eq. .true.) then
            do_cmpar = .false.
            qch = 'F'
          end if
          call gsn_putchar( INT2(5), INT2(10), qch )
!
!
!       else if (ich .eq. 54) then
        else if (cch(4:4) .eq. '6') then
!
          WRITE(qstr ,'("Do you want to skip a station?","(y/(n))",$)')
          call asnl(qstr )
          call senkr_mn (ix, iy, ich )
          call casefold (cch)
!         If(ich .eq. 58) then
          If(cch(4:4) .eq. ':') then
            call end_mn()
            stop
          end if
!
          kskip_stat = .false.
          qskip_stat = '        '
          qch = 'F'
!
!         IF(ich .eq. 89) then
          IF(cch(4:4) .eq. 'Y') then
            kskip_stat = .true.
            qch = 'T'
            WRITE(qstr ,'("Station to skip?  ",$)')
            call asnl(qstr )
            call getstr_f (qskip_stat )
          END IF
!
          ix = 20
          iy = 12
          call setcr_mn (ix, iy )
          call asnl(qskip_stat )
          call gsn_putchar( INT2(5), INT2(12), qch )
!
!
!       else if (ich .eq. 55) then
        else if (cch(4:4) .eq. '7') then
!
          if (do_min_sig .eq. .false.) then
            do_min_sig = .true.
            qch = 'T'
          else if (do_min_sig .eq. .true.) then
            do_min_sig = .false.
            qch = 'F'
          end if
          call gsn_putchar( INT2(5), INT2(14), qch )
!
!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
!
!       else if (ich .eq. 83) then
        else if (cch(4:4) .eq. 'S') then
!
          do_uen_cpl = .true.
          kskip_stat = .false.
!
            WRITE(qstr , &
     &      '("Enter DONE after the last station.")')
            call asnl(qstr )
            i = 0
            qcplstat = "        "
            qcpl_request = '        '
!
            do &
     &        while((qcpl_request.ne."DONE").and.(qcpl_request.ne."done"))
              i = i + 1
              WRITE(qstr , '("Station name? ")')
              call asnl(qstr )
              call getstr_f (qcpl_request )
              qcpl_stat(i) = qcpl_request
            end do !while
            ncplstat = i - 1
!
!         Sort the designated cpl sites alphabetically.
!         This new step is being added because in theory an unsorted list
!         might be printed out of alphabetical order in the iers_siteid_<>
!         and related SINEX files.  (In reality, there is no danger.
!         HRAS 085 is the only current and anticipated cpl site.)
!
          call ftqsort( qcpl_stat, ncplstat, INT2(8) )
!
          call setcr_mn (ix1, iy1 )
          call clrtobot_mn()
          WRITE(qstr , '("Stations selected are:")')
          call asnl(qstr )
          do i = 1, ncplstat
            WRITE(qstr , '(i2,2x,a)') i,qcpl_stat(i)
            call asnl(qstr )
          end do
          call return_to_continue()
          call refresh_mn()
          krenew = .true.
!
!          write (qstr ,'("Are these OK? ((y)/n)")')
!         call asnl(qstr)
!          call senkr_mn (ix, iy, ich)
!          if (ich .eq. 78) then
!           i = 1
!           do while ((qstr(1:1) .ne. ":").and.(i .ne. 0))
!              write (qstr ,'("Numer of station to change?",
!     &                   "  (0 to quit)")')
!             call asnl(qstr)
!              call getstr_f(qstr)
!             read (qstr,*) i
!              WRITE(qstr , '("Station name? : to skip")')
!             call asnl(qstr)
!             if (qstr(1:1) .ne. ":") then
!                qcpl_stat(i) = qstr(1:8)
!c            end if
!           end do !while
!          end if !((ich .eq. 78)
!
!
!       else if (ich .eq. 82) then
        else if (cch(4:4) .eq. 'R') then
          krenew = .true.
!
!
!       else if (ich .eq. 79) then
        else if (cch(4:4) .eq. 'O') then
          k1_1 = .true.
          kdone = .true.
          call setcr_mn(ix1, iy1 )
          call clrtobot_mn()
!
!
!       else if (ich .eq. 66) then
        else if (cch(4:4) .eq. 'B') then
          kdone = .true.
          call setcr_mn(ix1, iy1 )
          call clrtobot_mn()
          call refresh_mn()
!
!
!       else if (ich .eq. 77) then
        else if (cch(4:4) .eq. 'M') then
          kdon2 = .true.
          kdone = .true.
          call setcr_mn(ix1, iy1 )
          call clrtobot_mn()
          call clear_mn()
          call refresh_mn()
!
!
!       else if (ich .eq. 84) then
        else if (cch(4:4) .eq. 'T') then
            write (qstr ,'("Sure you want to terminate now? (y/n)")')
            call asnl(qstr )
            call senkr_mn (ix, iy, ich )
            call casefold(cch)
!           if  (ich .eq. 89) then
            if  (cch(4:4) .eq. 'Y') then
              call end_mn()
              stop
            end if
!
        else
          call beep_mn()
!
        end if
!
!
      end do
!
!     Check if anything was selected, warn user if not.
!
      if ((.not. &
     &    do_xyz_vel).and.(.not. &
     &    k1_1).and.(do_annual .eq. &
     &    .false.).and.(do_uen_vel .eq. &
     &    .false.).and.(do_uen_adj .eq. &
     &    .false.).and.(do_xyz_cpl .eq. &
     &    .false.).and.(do_cmpar   .eq. &
     &    .false.).and.(do_min_sig .eq. &
     &    .false.).and.(do_uen_cpl .eq. &
     &.false.))then
        WRITE(qstr ,'("No tables have been selected!")')
        call beep_mn()
        call asnl(qstr )
        call return_to_continue()
!
!
      ENDIF
!
!
      return
      end
