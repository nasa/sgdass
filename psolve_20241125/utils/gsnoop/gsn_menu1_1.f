      SUBROUTINE gsn_menu1_1 (kdon2, k1_2)
!
!     Other output menu
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
      INCLUDE 'gsnoop_com.i'
!
      LOGICAL*2 kdone, kdon2, krenew, k1_2
!
      integer*4    ix, iy, ich
      character*4 cch
      equivalence (ich, cch)
      character*1  qch
      character*80 qstr
!
!
!     Added  (m) Make tables now option.  Separated global positions and
!     velocities   DS Caprette  HSTX 93/08/10
!
!     Converted to curses DS Caprette HSTX 94/07
!
!     ability to generate the 6 1995 iers site submission tables. KDB 4/11/95
!
!     Add new file, lu 98, as "uen_sigs_<>" (Change menu wording for other
!        page option 3, which activates the file).  K. Baver 11/20/95
!
!     New option, to automatically generate plots of the type requested by
!        Jim Ryan for the 1995 IUGG meeting.  K. Baver 12/29/95
!
!     Due to lack of interest and difficulty of use, disable option installed
!        on 12/29/95.  (The option was supposed to automate production of
!        velocity plots, but these plots must be hand-edited.)  K. Baver 9/1/00
!
!     Sinex production is now done through solve, not gsnoop. So the three
!        options that generate sinex files will now be disabled to avoid wasting
!        time by porting them to linux.  K. Baver 4/26/06
!
!
!    Senkr_mn handles its input via an integer*4 variable, which
!                is subject to linux endian conversion problems.  So 
!                this subroutine will now transfer senkr_mn's input to a 
!                character variable and work with that.   K. Baver 3/23/06
!
      k1_2  = .false.
      kdon2 = .false.
      kdone = .false.
      krenew = .true.
!
!
!
 110  do while (.not. kdone)
        if (krenew) then
          ix = 0
          iy = 0
          call setcr_mn (ix, iy )
          call clrtobot_mn()
          call refresh_mn()
!
!
!
!
!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
!
         qstr= &
     &   "SNOOP output menu.                                   94/07"
         call asnl(qstr )
!
         write (qstr, &
     &         '("(1) ",l, " Site latitudes and longitudes")')do_lat_lon
         call as2nl(qstr )
!
!        write (qstr, '("(2) ",l, " Map files             ",
!    &                  "(a) ",l, " automatic gmt maps")')
!    &         do_hyper,do_auto_gmt_jwr
         write (qstr, '("(2) ",l, &
     &                                                 " Map files            &
     &         ")')do_hyper
         call as2nl(qstr )
!
         write (qstr, &
     &         '("(3) ",l, " UEN sigmas and BIN")')bin_uen_sigs
         call as2nl(qstr )
!
         write (qstr, &
     &         '("(4) ",l, " IERS sites")')do_iers_site
         call as2nl(qstr )
!
         write (qstr, &
     &         '("(5) ",l, " IERS sources")')do_iers_source
         call as2nl(qstr )
!
         write (qstr, &
     &         '("(6) ",l, " Sites flyby (mod file)")')site_flyby
         call as2nl(qstr )
!
         write (qstr, '( &
     &"(7) ",l, &
     &   " Sources flyby (mod)                                   ")') source_flyby
         call as2nl(qstr )
!
         write (qstr, '( "    ")')
         call as2nl(qstr )
!
         write (qstr, '( "    ")')
         call as2nl(qstr )
!
         qstr = &
     &   "(m) Make tables now         (p) positions and velocities menu"
         call as2nl(qstr )
!
         qstr = &
     &   "(b) Return to main menu     (t) terminate program    "// &
     &   "(r) Refresh Screen"
         call as2nl(qstr )
!
          call refresh_mn()
          krenew = .false.
        end if
!
!       Clear infterface area, sense response, and set flags.
!
        ix = 1
        iy = 22
        call setcr_mn(ix, iy )
        call clrtobot_mn()
        call senkr_mn(ix, iy, ich )
        call casefold(cch)
!
!
!       if (ich .eq. 49) then
        if (cch(4:4).eq.'1') then
!
          if (do_lat_lon .eq. .false.) then
            do_lat_lon = .true.
            qch = 'T'
          else if (do_lat_lon .eq. .true.) then
            do_lat_lon = .false.
            qch = 'F'
          end if
          call gsn_putchar   ( INT2(5), INT2(1), qch )
!
!
!       else if (ich .eq. 50) then
        else if (cch(4:4).eq.'2') then
!
          if (do_hyper .eq. .false.) then
            do_hyper = .true.
            qch = 'T'
          else if (do_hyper .eq. .true.) then
            do_hyper = .false.
            qch = 'F'
          end if
          call gsn_putchar   ( INT2(5), INT2(3), qch )
!
!
!       else if (ich .eq. 51) then
        else if (cch(4:4).eq.'3') then
!
          if (bin_uen_sigs .eq. .false.) then
            bin_uen_sigs = .true.
            qch = 'T'
          else if (bin_uen_sigs .eq. .true.) then
            bin_uen_sigs = .false.
            qch = 'F'
          end if
          call gsn_putchar   ( INT2(5), INT2(5), qch )
!
!
!       else if (ich .eq. 52) then
        else if (cch(4:4).eq.'4') then
!
          if (do_iers_site .eq. .false.) then
            do_iers_site = .true.
            qch = 'T'
          else if (do_iers_site .eq. .true.) then
            do_iers_site = .false.
            qch = 'F'
          end if
          call gsn_putchar   ( INT2(5), INT2(7), qch )
!
!
!       else if (ich .eq. 53) then
        else if (cch(4:4).eq.'5') then
!
          if (do_iers_source .eq. .false.) then
            do_iers_source = .true.
            qch = 'T'
          else if (do_iers_source .eq. .true.) then
            do_iers_source = .false.
            qch = 'F'
          end if
          call gsn_putchar   ( INT2(5), INT2(9), qch )
!
!
!       else if (ich .eq. 54) then
        else if (cch(4:4).eq.'6') then
!
          if (site_flyby .eq. .false.) then
            site_flyby = .true.
            qch = 'T'
          else if (site_flyby .eq. .true.) then
            site_flyby = .false.
            qch = 'F'
          end if
          call gsn_putchar   ( INT2(5), INT2(11), qch )
!
!
!       else if (ich .eq. 55) then
        else if (cch(4:4).eq.'7') then
!
          if (source_flyby .eq. .false.) then
            source_flyby = .true.
            qch = 'T'
          else if (source_flyby .eq. .true.) then
            source_flyby = .false.
            qch = 'F'
          end if
          call gsn_putchar   ( INT2(5), INT2(13), qch )
!
!       else if (ich .eq. 56) then !chose (8)
!       else if (cch(4:4).eq.'8') then
!         if (do_sinex_non .eq. .false.) then
!           do_sinex_non = .true.
!           qch = 'T'
!         else if (do_sinex_non .eq. .true.) then
!           do_sinex_non = .false.
!           qch = 'F'
!         end if
!         call gsn_putchar   ( INT2(5), INT2(15), qch )
!
!       else if (ich .eq. 57) then !chose (9)
!       else if (cch(4:4).eq.'9') then
!         if (do_sinex_cov .eq. .false.) then
!           do_sinex_cov = .true.
!           qch = 'T'
!         else if (do_sinex_cov .eq. .true.) then
!           do_sinex_cov = .false.
!           qch = 'F'
!         end if
!         call gsn_putchar   ( INT2(5), INT2(17), qch )
!
!       else if (ich .eq. 48) then !chose (0)
!       else if (cch(4:4).eq.'0') then
!         if (do_sinex_cap .eq. .false.) then
!           do_sinex_cap = .true.
!           qch = 'T'
!         else
!           do_sinex_cap = .false.
!           qch = 'F'
!         end if
!         call gsn_putchar   ( INT2(34), INT2(17), qch )
!
!       else if (ich .eq. 65) then !chose (a)
!         if (do_auto_gmt_jwr .eq. .false.) then
!           do_auto_gmt_jwr = .true.
!             qch = 'T'
!         else
!           do_auto_gmt_jwr = .false.
!             qch = 'F'
!         end if
!  call gsn_putchar   (34, 3, qch)
!
!       else if (ich .eq. 77) then
        else if (cch(4:4).eq.'M') then
          kdon2 = .true.
          kdone = .true.
          ix = 0
          iy = 0
          call setcr_mn(ix, iy )
          call clrtobot_mn()
          call refresh_mn()
!
!       else if (ich .eq. 82) then
        else if (cch(4:4).eq.'R') then
          krenew = .true.
          ix = 0
          iy = 0
          call setcr_mn(ix, iy )
          call clrtobot_mn()
          call clear_mn()
          call refresh_mn()
!
!       else if (ich .eq. 80) then
        else if (cch(4:4).eq.'P') then
          k1_2 = .true.
          kdone = .true.
          ix = 0
          iy = 0
          call setcr_mn(ix, iy )
          call clrtobot_mn()
          call clear_mn()
          call refresh_mn()
!
!       else if (ich .eq. 66) then
        else if (cch(4:4).eq.'B') then
          kdone = .true.
          ix = 0
          iy = 0
          call setcr_mn(ix, iy )
          call clrtobot_mn()
          call clear_mn()
          call refresh_mn()
!
!       else if (ich .eq. 84) then
        else if (cch(4:4).eq.'T') then
            write (qstr ,'("Sure you want to terminate now? (y/n)")')
            call asnl(qstr )
            call senkr_mn (ix, iy, ich )
            call casefold (cch)
!           if  (ich .eq. 89) then
            if  (cch(4:4).eq.'Y') then
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
      if ((do_vel .eq. &
     &  .false.).and.(k1_2 .eq. &
     &  .false.).and.(do_hyper .eq. &
     &  .false.).and.(do_annual .eq. &
     &  .false.).and.(site_flyby .eq. &
     &  .false.).and.(do_lat_lon .eq. &
     &  .false.).and.(source_flyby .eq. &
     &  .false.).and.(bin_uen_sigs .eq. &
     &  .false.).and.(do_iers_site .eq. &
     &  .false.).and.(do_iers_source .eq. &
     &  .false.).and.(do_sinex_non .eq. &
     &  .false.).and.(do_sinex_cov .eq. &
     &  .false.).and.(do_sinex_cap .eq. &
     &  .false.).and.(do_auto_gmt_jwr .eq. &
     &.false.))then
        WRITE(qstr ,'("No tables have been selected!")')
        call asnl(qstr )
        call return_to_continue()
!
!
      ENDIF
!
!     doing automatic gmt maps option requires files generated by the map files
!     option
!
      if (do_auto_gmt_jwr) do_hyper = .true.
!
!
      return
      end
