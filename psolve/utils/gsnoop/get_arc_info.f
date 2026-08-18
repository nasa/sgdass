      SUBROUTINE get_arc_info()
!
!     Subroutine to get mean epoch & span of obsevations for stations
!     and sources in the spoolfile.
!     DS Caprette 3/9/91
!     Stations list should be sorted befor coming here, otherwise you
!     might not be very happy with the results!
!     Added proper handling of sites with episodic motion
!     DS Caprette 92/03/24
!     Changed mjd from date of first atmosphere to date of first PM
!     DS Caprette 93/03/24
!     Set mean mjd in IERS source file to 0, not 99999.9, if there is no data.
!     K. Baver 2/27/95
!     Produce solution_epoch part of 1995 iers site submission on an informal
!     basis.  (This requires precision down to the minute in the internal
!     julian date variable, mjd.) K. Baver 3/30/95
!     Bring setting of mjd variable up to date (to take into account
!     solutions in which the wobble epoch is halfway through the experiment,
!     not at midnight.) K. Baver 4/95
!     For iers source table, optionally use only sessions with good
!     observations for the output epochs and session count.  K. Baver 9/29/95
!     Read into common information needed for a new IERS site submission file,
!     the solution statistics section.  K. Baver 4/24/96
!     If the value in the chi square field of the spoolfile's
!     overall statistics section is too large (*'s), set the chi square to 1
!     instead of aborting.
!     K. Baver 7/30/96
!     Handle spoolfiles without the overall statistics section
!     (e.g., spoolfiles from forward solutions).
!     K. Baver 3/6/97
!     Raise max_sta_exp from 160 to 992 (twice the maximum baselines per arc,
!     old value vs. new).
!     K. Baver  8/6/97
!     Collect info for continuous piecewise linear sites for placement in the
!         iers_solep_<> file. K. Baver  8/6/98
!     If ista_sum_dlay(j) is 0 for site j, this indicates that no arc was found
!     for site j.  This in turn generally indicates that some arcs are missing.
!     (Usually the user has done a single arc back solution.)
!     With ista_sum_dlay(j) 0, no sta_mean_epoch can be calculated for that
!     site, and in turn no span data can be calculated for the site.  This is
!     a problem for sinex submissions, which need the span data.
!     So warn the user of this problem and the fact that he needs all arcs
!     for a sinex submission.  But let him forge on anyway, in case
!       a) he's not doing a sinex submission
!       b) he wants to do a partial sinex solution, as in 10/98 when Chopo Ma
!          wanted to quickly get at just the solest/ap and solmat/ap files.
!     K. Baver 10/22/98
!     Y2K fixes.
!       K. Baver 11/23/98
!     Only flag missing arcs if producing sinex files.
!       K. Baver 1/7/99
!     Only extract the IERS "number of unknowns" and "number of observations"
!       if an IERS sinex report is being prepared.
!       K. Baver 6/9/99
!     Clarify confusing question: (include sessions without good source data?)
!       K. Baver 9/14/00
!     Make compatible with f_solve:
!       1. if no rates, f_solve prints No rate instead of Rate 0 in cres
!          general statistics section.
!       2. Some f_solve modes print N/A instead of a delay chi sq in the
!          1Overall section.
!            K. Baver 9/18/00
!     F_solve provides a fix for arcs that didn't estimate eop parameters:
!        F_solve adds a new line:  EOP epoch (UTC)  MJD: #####.#####
!        This is the same epoch as the one in the X Wobble parameter line,
!          just expressed to a greater precision, so if the X Wobble line
!          isn't there, look for the new line.
!        (Do not just look for the new line in place of the X Wobble line.
!         This must be backwards compatible with s_solve spoolfiles.)
!        Note: this new line will not be added if segmented eop parameters are
!              estimated, so if such a spoolfile is ever used, a new fix must
!              be found.
!            K. Baver 10/6/00
!        Change max_src to max_src_gsnoop to avoid a conflict with solve.i's
!          max_src.
!        Also change max_sta to max_sta_gsnoop.
!            K. Baver 10/25/00
!     The parameter index in solve spoolfile adjustments is
!       being enlarged from a four character field to five characters.
!       Update to read the new format.  K. Baver.  5/2002
!     The EOP epoch (UTC)  MJD: xxxxx.xxxxx line is being changed to:
!   EOP epoch (TDT)  MJD: 52411.270833        NUT epoch (TDT)  MJD: 52411.265045
!       to use Terrestrial Dynamic time, not UTC.
!       Update to accomodate this change.  K. Baver. 6/3/2002
!     Change from separate changeover variables to an array.
!       Add julian date changeover array to common.  K. Baver 1/9/2003
!     Make various changes to update this subroutine to be able to handle the
!       current spoolfile format output by PET.
!         1.  Add the ability to handle spoolfiles whose source statistics 
!             sections have the SRC_STAT line format.
!         2.  As of 06.03.29, some spoolfiles have parameter epochs
!             with four digit years and precision to 1/1000 of a second.
!             Add the ability to handle this format.
!         3.  Convert parsing of the number of observations used for the rate
!             case from column oriented to field oriented parsing.
!       K. Baver 6/10/08
!
!
      IMPLICIT NONE
!
      INCLUDE 'gsnoop_com.i'
      INCLUDE 'solve.i'
!
      INTEGER*4     IOS
      INTEGER*2     I, J, M
      Integer*4     iexp_tot_dlay, iexp_tot_rate
      Integer*4     ix, iy
!
      integer*2     jyr, jmn, jdy, jhour, jminute
      integer*2     nexp, ista, isrc
      integer*2     oldtotsta, oldtotsrc
      integer*2     no_data
      integer*2     max_sta_exp
      parameter     (max_sta_exp = 992) !twice max # baselines possible
!                                         in one experiment
      integer*4     jexpdate, jdate
!
      real*8        rchi
      real*8        xjunk
      real*8        mjd, fjldy
      integer*4     iexp_sta_used(max_sta_exp), &
     &              ista_sum_dlay(max_sta_gsnoop)
      real*8        sta_sum_wmjd(max_sta_gsnoop)
!
      integer*2     iexp_src_dlay(max_src_gsnoop)
      real*8        src_sum_wmjd(max_src_gsnoop)
!
      character*8   station(max_sta_exp)
      character*8   source(max_src_gsnoop), staname(max_sta_gsnoop)
      character*80  qstr
      logical*2     first_mjd
      character*1   cepoch_add
      integer*2     trimlen
      integer*2     ict
      real*8        tmp_num_constr
      integer*2     ierr
      logical*2     overall_stat
      integer*2 nct,npt,mct,ipt,interval
      logical*2 missing_arcs
      character*1 missing_forge
      integer*2 jyr_dig4,icpls_year_dig2,icpls_year_dig4
      character*1 no_chi_forge
      real*8 jd_eop_epoch,mjd_fract
      integer*2 icent
      integer*2 icol_adj
      character*150 cbuf_working
      character*25 cdummy
      integer*2 iyr_offset
!
!
!     Initialize sums etc
!
!
      totsta = 0
      totsrc = 0
      oldtotsta = 0
      oldtotsrc = 0
      no_data = 0
!
      do i = 1, max_sta_exp
        iexp_sta_used(i)  = 0
      end do
!
      do i = 1, max_sta_gsnoop
        staname(i)        = "        "
        ista_sum_dlay(i)  = 0
        sta_sum_wmjd(i)   = 0.0d0
        sta_mjdmax(i)     = 0.0d0
        sta_mjdmin(i)     = 1.0d8
        sta_mean_epoch(i) = 0.0d0
        sta_span(i)       = 0.0d0
      end do
!
      do nct = 1,ncplstat
        do mct = 1, cpls_nepochs(nct)+1
          cpls_sum_dlay(mct,nct) = 0
          cpls_sum_wmjd(mct,nct) = 0.0D0
          cpls_max(mct,nct) = 0.0d0
          cpls_min(mct,nct) = 1.0d8
          cpls_mean(mct,nct) = 0.0d0
        enddo
      enddo
!
      do i = 1, max_src_gsnoop
        srcname(i)        = "        "
        iexp_src_dlay(i)  = 0
        src_sum_dlay(i)   = 0.0d0
        src_sum_wmjd(i)   = 0.0d0
        src_sum_rate(i)   = 0.0d0
        src_mjdmax(i)     = 0.0d0
        src_mjdmin(i)     = 1.0d8
        src_mean_epoch(i) = 0.0d0
        src_sum_sess(i)   = 0.0d0
        src_sum_good_sess(i)   = 0.0d0
        src_span(i)       = 0.0d0
      end do
!
      icol_adj = 0
      if (spool_solve_revision_date_jd .ge. changeover_jd(1)) &
     & icol_adj = 1
!
      first_mjd = .true.
      ix = 1
      iy = 8
      call setcr_mn(ix, iy)
      qstr ="Getting mean epoch information.        "
      call as2nl(qstr)
      rewind(40) !rewind before next read into cbuf
!
!     See which sessions to use in accumulating information (mean, first
!     and last epoch and number of sessions) for a given source
!     in the iers source table.  The choices are all sessions in which the
!     source appears or just those for which the source had good data.
!
      if (do_iers_source) then
        qstr = 'Include sessions without good source data '
        call asnl(qstr)
        qstr = '  (i.e., ones that tried but failed to observe '// &
     &             'a given source)'
        call asnl(qstr)
        qstr = '  (Y)(default)/(N)?'
        call asnl(qstr)
        call getstr_f(qstr)
        read(qstr,"(a1)") use_bad_src_sess
        call casefold(use_bad_src_sess)
        if (trimlen(use_bad_src_sess).eq.0) use_bad_src_sess = 'Y'
        if (use_bad_src_sess.ne.'Y'.and.use_bad_src_sess.ne.'N') &
     &        use_bad_src_sess = 'Y'
      endif
!
      Do While(cbuf(1:8) .ne. '1Overall')
!
!       Look for the baseline statistics
!
        do while (CBUF(1:8) .ne. '   Delay')
          read(40,'(a)',end = 999) cbuf
          if (cbuf(1:8) .eq. "1Overall")  go to 999 !yes a 'go to'!
        end do
!
        nexp = nexp + 1
        read (cbuf(13:20), *) xjunk
        iexp_tot_dlay = dint(xjunk)
        read(40,'(a)', end=999) cbuf
        if (index(cbuf,"No rate").ne.0) then
          xjunk = 0.0D0
        else
!         Get number of observations used (for the rate)
          cbuf_working = cbuf
          call splitstring(cbuf_working,cdummy,cbuf_working)
          call splitstring(cbuf_working,cdummy,cbuf_working)
          read (cdummy, *) xjunk
        endif
        iexp_tot_rate = dint(xjunk)
!
        do while (CBUF(1:20) .ne. ' Baseline Statistics')
          read(40,'(a)', end=999) cbuf
        end do
!
!
        do i = 1,4     ! read up to and including first baseline
          READ(40,'(A)',END=999) CBUF
        end do
!
!         Fill the array of station names for this experiment.
!         Pick up the # observations used for each.  Remember
!         two stations per baseline so exp_stat_obs(1) corresponds
!         to both station(1) and station(2) etc
!         Each station gets counted one for each baseline it is in.
!
        ista = 0
        do while (cbuf(1:9) .ne. "        ")
          ista = ista + 2
          read(cbuf(2:9),'(a)')   station(ista-1)    !#'s 1,3,5,7,9
          read(cbuf(11:18),'(a)') station(ista)      !#'s 2,4,6,8,10
!
          if (cbuf(21:27) .ne. 'No Data') then
            read(cbuf(19:22),*)     xjunk
            iexp_sta_used(ista)   = dint(xjunk)
            iexp_sta_used(ista-1) = iexp_sta_used(ista)
          else
            iexp_sta_used(ista)   = 0
            iexp_sta_used(ista-1) = 0
          end if
!
          if (iexp_sta_used(ista) .eq. 0) then !No data used
            ista = ista - 2   !So ignore this baseline
            no_data = no_data + 1
          end if
!
          READ(40,'(A)',END=999)   CBUF
        end do
!
!       Read past header lines to reach the first source statistic line.
!       Allow for a variable number of header lines, but assume that the 
!       header lines and the source statistic lines will be separated by one
!       blank line.
!
        do while (CBUF(1:18) .ne. ' Source Statistics')
          read(40,'(a)', end=999) cbuf
        end do

        READ(40,'(A)',END=999) CBUF
        do while (trimlen(cbuf).gt.0)
          READ(40,'(A)',END=999) CBUF
        end do
 
        READ(40,'(A)',END=999) CBUF

!
!         Fill the array of source names for this experiment.
!         Pick up the # observations used for each.
!
        isrc = 0
        do while (cbuf(6:14) .ne. "        ")
          isrc = isrc + 1
          if (cbuf(1:8).eq.'SRC_STAT') then
!           new format
            read(cbuf(12:19),'(a)') source(isrc)
            read(cbuf(21:25),*) iexp_src_dlay(isrc)
          else
!           old format
            read(cbuf(6:13),'(a)')  source(isrc)
            read(cbuf(17:22),*)     xjunk
            iexp_src_dlay(isrc) = dint(xjunk)
          endif
          READ(40,'(A)',END=999)   CBUF
        end do
!
!         Read down to experiment date and pick it up.
!
!        do i = 1,14
!          READ(40,'(A)',END=999) CBUF
!        end do
!
!       If we find a clock grab that date in case there are no EOP
!       records.  Then look for first EOP record in experiment.
!
!       If we succeed, use it.
!       If not, look for a new EOP epoch (UTC)  MJD: or
!           EOP epoch (TDT)  MJD: line printed by f_solve
!          and use that.
!       If we get to "1Run" we know there were no "Wobbles". Use the clock
!         epoch, but flag the case as pathological.
!
        do while ((cbuf(9+icol_adj:14+icol_adj) .ne. "Wobble") .and. &
     &            (cbuf(1+icol_adj:21+icol_adj) .ne. &
     &                 "EOP epoch (UTC)  MJD:") .and. &
     &            (cbuf(1+icol_adj:21+icol_adj) .ne. &
     &                 "EOP epoch (TDT)  MJD:") .and. &
     &            (CBUF(1:4) .ne. '1Run'))
          READ(40,'(A)',END=999) CBUF
          if (cbuf(16+icol_adj:17+icol_adj) .eq. 'CL') then
!
!           There are now two possible formats for the clock epoch:
!                    yy/mm/dd hh:mm          or
!                    yyyy.mm.dd-hh:mm:ss.sss
!
            if (index(cbuf(21+icol_adj:34+icol_adj),'.').ne.0) then
              iyr_offset = 2
            else
              iyr_offset = 0
            endif
            read(cbuf(21+icol_adj+iyr_offset:28+icol_adj+iyr_offset), &
     &         '(3(i2,1x))') jyr, jmn, jdy
          end if
        end do
!
        if (cbuf(9+icol_adj:14+icol_adj) .eq. "Wobble") then  !found eop record
!
!         There are now two possible formats for the wobble epoch:
!                  yy/mm/dd hh:mm          or
!                  yyyy.mm.dd-hh:mm:ss.sss
!
          if (index(cbuf(21+icol_adj:34+icol_adj),'.').ne.0) then
            iyr_offset = 2
          else
            iyr_offset = 0
          endif
          read(cbuf(21+icol_adj+iyr_offset:35+icol_adj+iyr_offset), &
     &       '(5(i2,1x))') jyr, jmn, jdy, jhour, jminute
          call newcents(jyr,jyr_dig4)
          jexpdate = 10000*jyr_dig4 + 100*jmn + jdy
!         Get the julian date at midnight on the day of the wobble epoch.
!         Strip off 2400000 which is common to all julian dates at the moment.
          mjd = fjldy(jmn, jdy, jyr) - 2400000.0D0
!         Check to see if this solution's wobble epochs occurred at midnight
!         (the old case) or at the experiments' midpoints.  If the first
!         experiment's epoch occurred at some time other than midnight,
!         gsnoop will assume that every experiment's epoch will have occurred
!         at the midpoint.  However, if the first epoch is at midnight,
!         the situation is ambiguous and
!         gsnoop will just ask which case it is dealing with.
          if (first_mjd) then
            if (jminute .eq. 0 .and. jhour .eq. 0) then
              write(qstr,"('The first experiment has a wobble epoch ', &
     &          'at midnight')")
              call asnl(qstr)
              write(qstr,"('Should gsnoop assume all epochs are ', &
     &          'at midnight ')")
              call asnl(qstr)
              write(qstr,"('and add half a day to each one ')")
              call asnl(qstr)
              write(qstr,"('Y (default)/ N ? ')")
              call asnl(qstr)
              call getstr_f(cepoch_add)
              call casefold (cepoch_add)
              if (cepoch_add.ne.'N') cepoch_add = 'Y'
            else
              cepoch_add = 'N'
            end if
            first_mjd = .false.
          end if
          if (cepoch_add.eq.'Y') then
!           The user wants to assume all epochs are midnight ones (the old
!           style and add half a day to each to convert them to noon.
            mjd = mjd + 0.5D0
          else
!           The epochs are at the midpoints of the experiments (or else the
!           user has asked to assume they are).  Get the julian date at
!           the exact epoch (to the nearest minute).
            mjd = mjd + jminute / 1440.0D0 + jhour / 24.0D0
          end if
        else if ((cbuf(1+icol_adj:21+icol_adj) .eq. &
     &                "EOP epoch (UTC)  MJD:") .or. &
     &           (cbuf(1+icol_adj:21+icol_adj) .eq. &
     &                "EOP epoch (TDT)  MJD:")) &
     &            then
!         Use new f_solve eop epoch record
          read (cbuf(23+icol_adj:33+icol_adj),"(f11.5)") mjd
!         Add .5 because the mjd pulled from the spoolfile is true mjd format
!         (julian date minus 2400000.5) whereas the value in gsnoop variable
!          mjd should be just jd minus 2400000 at this point.
          mjd = mjd + 0.5D0
!         Generate experiment date in form for internal comparison,
!           using year, month, day.
          jd_eop_epoch = mjd + 2400000.0D0
          call mdyjl(jmn,jdy,jyr,icent,jd_eop_epoch)
          call newcents(jyr,jyr_dig4)
          jexpdate = 10000*jyr_dig4 + 100*jmn + jdy
!         Check for need to shift midnight epochs to noon epochs.
          if (first_mjd) then
            mjd_fract = mjd - dint(mjd)
            if (dabs(mjd_fract - 0.5D0) .lt. 0.00001D0) then
!             The mjd fraction is less than a second away from midnight,
!             so the mjd must be at midnight.
              write(qstr,"('Experiment one has a wobble epoch ', &
     &          'at midnight')")
              call asnl(qstr)
              write(qstr,"('Should Gsnoop assume all epochs are ', &
     &          'at midnight ')")
              call asnl(qstr)
              write(qstr,"('and add half a day to each one ')")
              call asnl(qstr)
              write(qstr,"('Y (default)/ N ? ')")
              call asnl(qstr)
              call getstr_f(cepoch_add)
              call casefold (cepoch_add)
              if (cepoch_add.ne.'N') cepoch_add = 'Y'
            else
              cepoch_add = 'N'
            end if
            first_mjd = .false.
          end if
          if (cepoch_add.eq.'Y') then
!           The user wants to assume all epochs are midnight ones (the old
!           style) and add half a day to each to convert them to noon.
!           (Note: if the epochs are at the midpoints of the experiments (or
!                  the user has asked to assume they are), no action is
!                  necessary.  The mjd should already have the exact epoch.)
!
            mjd = mjd + 0.5D0
          endif
        else ! no wobble or EOP epoch record in experiment
          write (qstr,'("No wobble or EOP epoch in experiment ", &
     &                   "- mjd set to ", &
     &                   i2,"/",i2,"/",i2," by clock epoch")') &
     &                   jyr, jmn, jdy
          call as2nl(qstr)
          write (qstr,'("This is a pathological case ", &
     &          "which is not adequately handled. ")')
          call as2nl(qstr)
          write (qstr,'("Please report it to a programmer.")')
          call as2nl(qstr)
          write (qstr,'("return to continue")')
          call as2nl(qstr)
          call getstr_f(qstr)
          call newcents(jyr,jyr_dig4)
          jexpdate = 10000*jyr_dig4 + 100*jmn + jdy + 1
          mjd = fjldy(jmn, jdy, jyr) - 2399999.5d0 + 1
        end if
!
!         Look through the arrays of station information and accumulate
!         the information we need.  If a station is not yet in the list
!         staname, we add it to the bottom.
!
        do i = 1, ista   !loop through list of stations in experiment
          j = 1          !loop through list of stations already found
          do while ((staname(j) .ne. station(i)) &
     &      .and.(j .le. totsta))
            j = j + 1
          end do
!
          if (j .gt. totsta) then   !new entry
            staname(j) = station(i)
            totsta = j
          end if
!
!         Loop through list of stations in solution and find
!         entry(s) for same station.
!
          j = 1       !loop through list of stations in solution
          do while ((site_names(j)(1:8) .ne. station(i)) &
     &      .and.(j .le. nsite))
            j = j + 1
          end do
!
!         If station was not in site_names, (j .gt. nsite)
!         it must have been skipped when picking uyp the global
!         site info.  So, skip it again here.
!
          if (j .le. nsite) then
!
!           Check if station is episodic.  If so, get into the right
!           episode.
!
            if (site_names(j)(1:8) .eq. site_names(j+1)(1:8)) then
              read(site_names(j+1)(13:18),'(3i2)') jyr, jmn, jdy
              call newcents(jyr,jyr_dig4)
              jdate = 10000*jyr_dig4 + 100*jmn + jdy
              ios = 0
              do while ((jdate .le. jexpdate).and.(ios .eq. 0).and. &
     &                 (site_names(j)(1:8) .eq. site_names(j+1)(1:8)))
                j = j + 1
                read(site_names(j+1)(13:18),'(3i2)') jyr, jmn, jdy
                call newcents(jyr,jyr_dig4)
                jdate = 10000*jyr_dig4 + 100*jmn + jdy
              end do
!           else
!             site is not episodic, or we are in the last episode.
!             In either case, j is fine.
            end if
!
!           get the index
            read (site_names(j)(19:23), '(i5)') m
!
!           Accumulate sums, min max dates.
!
            ista_sum_dlay(m) = ista_sum_dlay(m) + iexp_sta_used(i)
            sta_sum_wmjd(m) = sta_sum_wmjd(m) + mjd*iexp_sta_used(i)
            if (sta_mjdmax(m) .lt. mjd) sta_mjdmax(m) = mjd
            if (sta_mjdmin(m) .gt. mjd) sta_mjdmin(m) = mjd
          end if ! (j .le. nsite)
        end do
!
!       Check for continuous piecewise linear sites and accumulate
!       their statistics.
!
        do ipt = 1, ista   !loop through list of stations in experiment
            nct = 0
            do npt = 1,ncplstat
              if (qcpl_stat(npt) .eq. station (ipt)) nct = npt
            end do
!
            if (nct.ne.0) then
!
!             This is a continuous piecewise linear site.
!             Get to the right epoch.
!             Accumulate data for n+1 epochs.  (That is, if n epochs, need n+1
!                intervals.)
!
!                    interval before epoch1 = interval 1
!                       epoch 1
!                    interval before epoch2 = interval 2
!                       epoch 2
!                    interval before epoch3 = interval 3
!                       epoch 3
!                    interval before epoch4 = interval 4
!                       epoch 4
!                    data after all epochs = interval 5
!
!
              interval = 1
              icpls_year_dig2 = icpls_date(nct,1,interval)
              call newcents(icpls_year_dig2,icpls_year_dig4)
              jdate = 10000*icpls_year_dig4 + &
     &                   100*icpls_date(nct,2,interval) + &
     &                       icpls_date(nct,3,interval)
              do while (jdate.le.jexpdate .and. &
     &                interval .le. cpls_nepochs(nct))
                interval = interval + 1
                if (interval.le.cpls_nepochs(nct)) then
                  icpls_year_dig2 = icpls_date(nct,1,interval)
                  call newcents(icpls_year_dig2,icpls_year_dig4)
                  jdate = 10000*icpls_year_dig4 + &
     &                   100*icpls_date(nct,2,interval) + &
     &                       icpls_date(nct,3,interval)
                endif
              enddo
!
!             Accumulate sums, min max dates.
!
              cpls_sum_dlay(interval,nct) = cpls_sum_dlay(interval,nct) &
     &               + iexp_sta_used(ipt)
              cpls_sum_wmjd(interval,nct) = cpls_sum_wmjd(interval,nct) &
     &               + mjd*iexp_sta_used(ipt)
              if (cpls_max(interval,nct) .lt. mjd) &
     &            cpls_max(interval,nct) = mjd
              if (cpls_min(interval,nct) .gt. mjd) &
     &            cpls_min(interval,nct) = mjd
            endif
        end do
        if ((totsta - oldtotsta) .ge. 1) then
          ix = 1
          iy = 9
          if (.not. kdebug) call setcr_mn(ix, iy)
          write (qstr ,'("Total stations are ",i3)') totsta
          call as2nl(qstr)
          oldtotsta = totsta
        end if
!
        do i = 1, isrc   !loop through list of sources in experiment
          j = 1          !loop through list of sources in solution
          do while ((srcname(j) .ne. source(i)).and.(j .le. totsrc))
            j = j + 1
          end do
!
          if (j .gt. totsrc) then   !new entry
            srcname(j) = source(i)
            totsrc = j
          end if
!
!           Accumulate sums, min max dates.
!
          src_sum_sess(j) = src_sum_sess(j) + 1
          src_sum_dlay(j) = src_sum_dlay(j) + iexp_src_dlay(i) !#delays per src
          if (iexp_tot_dlay .eq. iexp_tot_rate) then
            src_sum_rate(j) = src_sum_rate(j) + iexp_src_dlay(i)
          end if
          src_sum_wmjd(j) = src_sum_wmjd(j) + mjd*iexp_src_dlay(i)
!
!         If doing all sessions OR doing only good sessions and this one's
!         good, increment the number of sessions in which the source appears
!         and update the minimum and maximum epochs.
!
          if (use_bad_src_sess.eq.'Y'.or.iexp_src_dlay(i).gt.0) then
            src_sum_good_sess(j) = src_sum_good_sess(j) + 1
            if (src_mjdmax(j) .lt. mjd) src_mjdmax(j) = mjd
            if (src_mjdmin(j) .gt. mjd) src_mjdmin(j) = mjd
          end if
        end do
        if ((totsrc - oldtotsrc) .ge. 1) then
          ix = 1
          iy = 10
          if (.not. kdebug) call setcr_mn(ix, iy)
          write(qstr ,'("Total sources are ",i4)') totsrc
          call as2nl(qstr)
          oldtotsrc = totsrc
        end if
        READ(40,'(A)',END=999) CBUF
      end do !while .ne. '1Overall'
!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
!
!
 999  if (cbuf(1:8).eq.'1Overall') then
!       This spoolfile has an overall statistics section, and we just found it.
        overall_stat = .true.
      else
!       This spoolfile has no overall statistics section, and we are now at
!       the end of file.
        overall_stat = .false.
      endif
      if (overall_stat) then
        do i = 1, 6
          read(40,'(a)', end=1999) cbuf
        end do
        if (index(cbuf(54:),"N/A").ne.0) then
!         F_solve change.  Some f_solve modes don't support chi sq.
          if (do_sinex_non .or. do_sinex_cov .or. do_sinex_cap) then
            qstr = 'Your f_solve_mode did not produce a chi sq, '
            call asnl(qstr)
            qstr = 'So I cannot do a proper sinex solution.'
            call asnl(qstr)
            qstr = 'Should I forge on anyway, '
            call asnl(qstr)
            qstr = 'setting the chi sq to 1?'
            call asnl(qstr)
            call getstr_f(qstr)
            read(qstr,"(a1)") no_chi_forge
            call casefold(no_chi_forge)
            if (no_chi_forge .eq. 'N') then
              stop
            endif
          endif
          rchi = 1.0D0
        else
          read(cbuf(60:), *,iostat=ios) rchi
          ierr = ios
          if (ierr.ne.0) rchi = 1.0D0
        endif
        rrchi = dsqrt(rchi)
      else
        rchi = 0
        rrchi = 0
      endif
!
      ix = 1
      iy = 11
      if (.not. kdebug) call setcr_mn(ix, iy)
 1999 write (qstr , '("Solution root reduced chi sq = ",f5.3)') rrchi
      call as2nl(qstr)
!
!     Calculate span and mean epoch for all the stations
!     and sources.
!
      missing_arcs = .false.
      do j = 1, nsite  !NOT totsta!
        sta_span(j) = sta_mjdmax(j) - sta_mjdmin(j) + 1.0d0
        if (ista_sum_dlay(j).gt.0) then
          sta_mean_epoch(j) = sta_sum_wmjd(j)/ista_sum_dlay(j)
        else
          sta_mean_epoch(j) = -1.0D0
          missing_arcs = .true.
        endif
      end do
      if (missing_arcs .and. &
     &     (do_sinex_non .or. do_sinex_cov .or. do_sinex_cap)) then
        qstr = 'Your spoolfile seems to have some arcs missing.'
        call asnl(qstr)
        qstr = 'So I cannot do a proper sinex solution.'
        call asnl(qstr)
        qstr = 'Should I forge on anyway, '
        call asnl(qstr)
        qstr = 'setting all bad mean epochs to 1.0D0?'
        call asnl(qstr)
        call getstr_f(qstr)
        read(qstr,"(a1)") missing_forge
        call casefold(missing_forge)
        if (missing_forge .eq. 'Y') then
          do j = 1, nsite
            if (sta_mean_epoch(j).lt.0.0D0) sta_mean_epoch(j)= 1.0D0
          enddo
        else
          stop
        endif
      endif
!
!     Don't forget continuous piecewise linear sites
!
      do j = 1,ncplstat
        do mct = 1,cpls_nepochs(j) + 1
          cpls_mean(mct,j) = cpls_sum_wmjd(mct,j)/cpls_sum_dlay(mct,j)
        enddo
      enddo
!
      do j = 1, totsrc
        src_span(j) = src_mjdmax(j) - src_mjdmin(j) + 1.0d0
        if (src_sum_dlay(j) .gt. 0) then
          src_mean_epoch(j) = src_sum_wmjd(j)/src_sum_dlay(j)
        else
          src_mean_epoch(j) = 0.0d0
        end if
!       If no minimum epoch given the source epoch selection criteria,
!       reset epoch to 0 to avoid output field overflow.
        if (src_mjdmin(j).gt.9999999.9D0) src_mjdmin(j) = 0.0D0
      end do
!
      write(qstr ,'("Total stations are ",i3)') totsta
      call as2nl(qstr)
      write(qstr ,'("Total sources are ",i4)') totsrc
      call as2nl(qstr)
!
!     Read into common the statistics needed for the solution/statistics section
!     of the IERS site submission.
!
!     The IERS variance factor is the VLBI chi-square delays value printed
!     by hausr.
!
      iers_var_factor = rchi
      if (do_sinex_non.and.overall_stat) then
        read(40,'(a)') cbuf !skip rate line
!
!       Get the first part of the number of observations needed for the IERS
!       submission.  The iers considers the number of observations to be the
!       number of observations (from the VLBI point of view) plus the number
!       of solution constraints.
!
        read(40,'(a)') cbuf
        read(cbuf,"(14X,I7)") iers_num_obs
!
!       Go past the number of local and total parameters per arc, which span
!       variable line lengths in different spool files.
!
        Do While(cbuf(1:40) .ne. &
     &      'Maximum total parameters in a single arc')
          read(40,'(a)') cbuf
        enddo
!
!       Now get the IERS "number of unknowns" (the total number of parameters,
!       in VLBI terms).
!
        do ict = 1,4
          read(40,'(a)') cbuf
        end do
        read(cbuf,"(27X,I7)") iers_unknowns
!
!       Finally get the second half of the IERS "number of observations",
!       the number of constraints.
!
        read(40,'(a)') cbuf
        read(cbuf,"(27X,F9.1)") tmp_num_constr
        iers_num_obs = iers_num_obs + tmp_num_constr
      else
        iers_num_obs = 0
        iers_unknowns = 0
      endif
!
      return
      end
