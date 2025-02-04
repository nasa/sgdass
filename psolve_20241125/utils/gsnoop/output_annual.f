      SUBROUTINE output_annual()
!
!     Creates tables of annual positions
!
!     modifications:
!
!     kdb   2/6/95  point to right antenna.dat
!     kdb   3/24/95 add solution/epochs section of the 1995 iers site
!                   submission, on an informal basis
!     kdb   4/7/95  Ability to print output files for the 1995 iers
!                   site submission on a formal basis.
!                   (that is, site/id, solution/epochs,
!                   solution/estimate,solution/apriori,
!                   solution/matrix_estimate l corr and
!                   solution/matrix_apriori l corr files)
!                   Subsequently upgraded to look up the
!                   input covariance files for the
!                   latter four files from the solution catalog.
!                   Subsequently upgraded to print variable constraint levels
!                   (based on the solution's control file) in the
!                   solution/estimate (and solution/apriori) sections)
!     95/07/12 kdb Add ability to use input covariance and control files
!                  that are not catalogued in a solution archiving system.
!     96/04/24 kdb Print new file for the 1996 iers site submission, the
!                  solution statistics section.
!     96/04/25 kdb The reference epochs on the site lines of the
!                  iers_sit_<> and iers_both_<> files were hardcoded to
!                  47161 (1/1/88).  Change them to the correct epoch which
!                  is  printed in these files at the start of the site list
!                  for each epoch in the files.
!     96/04/26 kdb Optionally sort the iers_siteid_<> and iers_solep_<> files
!                  by longitude.
!     96/07/08 kdb Expanding the types of CVRFxx files that can be handled.
!                  Now will handle site and eop parameters from a solution with
!                  COVARIANCES BY_ARC ALL.
!     96/07/18 kdb Add sinex header line and sinex input/history section.
!     96/07/19 kdb Add sinex site/receiver, site/antenna and site/eccentricity
!                  sections.
!     96/07/22 kdb Add sinex input/files section.
!     96/07/25 kdb No longer print R for VLBI technique in solution/epochs
!                  section.  This helps to convert from the sinex 0.04 version
!                  to the 0.05 version.
!     96/07/26 kdb For a one-arc solution,
!                  get the epochs for the solest and solestap files from
!                  the spool file's wobble line, not its station table.
!     98/02/27 kdb Move antenna.dat to /data1/solve_files for use by Solve.
!     98/07/30 kdb Upgrate to sinex 1.00.
!     98/08/06 kdb Output continuous piecewise linear sites at the end of the
!                  iers_solep_<>.
!     kdb 980807  Handle output to sinex_site_ant_<>,
!                                  sinex_site_ecc_<>,
!                                  sinex_site_recvr_<>
!                 for continuous piecewise linear sites.
!     kdb 981120  Y2K fixes.
!     kdb 000927  Pass cpl sites to solest, so that it can number the sites'
!                 epochs properly in the soln field of the iers_solest_<> and
!                 iers_solestap_<> files.
!     kdb 000928  Pass cpl site names to site_id_sect.  (Part of changes to
!                 create cpl output for the iers_siteid_<> file.)
!     kdb 000929  Until now, cpl output has been appended to the iers_solep_<>
!                 file, and the user has had to manually place the output at
!                 its proper position.  Fix this subroutine to properly
!                 position the cpl output.
!     kdb 001025  Parameterize antenna.dat directory.
!     kdb 001025  Change max_sta to max_sta_gsnoop.
!     kdb 001101  Use solve.i parameters for center identification
!                 in sinex_header_<>, sinex_inhist_<> and
!                 sinex_input_files_<> files.
!                 Also generate file_comment_all, file_ref_all,
!                   gps_phase_cent_all, input_ack_all and sinex_trailer files.
!                 Also rename local spool_dir variable to spool_directory
!                   to avoid conflict with solve.i parameter.
!     kdb 010914  Currently the iers_sit_<> file is only used for the quarterly
!                   solution, so put header text appropriate to that purpose
!                   at the top of the file.  (The text is a detailed
!                   explanation.)
!     kdb 060421  Replace the dint calls with jidint for the Fortran 90 
!                 compiler.  Calling dint on sta_mean_epoch and sta_span
!                 left these as real*8, which can be written to an integer
!                 format under Fortran 77 (doing a truncation via a trick), but
!                 which causes an overflow condition in Fortran 90 when written
!                 to an integer format.  Jidint does an actual conversion to 
!                 integer (integer*4).
!     kdb 060421  Fix headers that were messed up by the fortran 77 to 90
!                 conversion program.
!
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
      INCLUDE 'gsnoop_com.i'
      INCLUDE 'solve.i'
!
      integer*2     i, j, m
      integer*4     IOS
      INTEGER*2     msite
      integer*4     idoms, idomp
      parameter     (idoms = 0)
      parameter     (idomp = 0)
      integer*2     max_cpl_sites
      parameter     (max_cpl_sites =10)
      integer*4     irepoch
      real*8        rrepoch
      real*8        fjldy
      integer*2     i_jan
      integer*2     i_one
!
      character*80  qstr
      character*9   qdomes
      character*6   qepoch
      character*2   Warning
      character*1   qtype
!
      LOGICAL*2 kdone
      integer*2     isoln,isolns(max_sta_gsnoop)
      real*8        full_start, full_stop, full_mean
      character*12  sta_min_disp,sta_min_disps(max_sta_gsnoop)
      character*12  sta_max_disp,sta_max_disps(max_sta_gsnoop)
      character*12  sta_mean_disp
      integer*4     jmin_trunc
      logical*2     ap_choice
      integer*2 trimlen,ivlen,iclen
      character*1   sort_choice
      integer*2 covs_wanted
      integer*2 cons_override
      integer*2 ncovparms
      real*8        full_start_min, full_stop_max
      character*12  sta_min_disp_min
      character*12  sta_max_disp_max
      integer*2 covs_found
      character*30 sinex_descrip
      character*140 spool_directory
      character*14 spool_base
      integer*2 isp_blen,isp_dlen
      character*12 sta_first_disp,sta_all_disp
      integer*2 ict,mct
      real*8        full_cpls_max, full_cpls_min, full_cpls_mean
      character*12  cpls_min_disp(ncpls+1,mcpl), &
     &              cpls_max_disp(ncpls+1,mcpl), &
     &              cpls_mean_disp
      integer*2 epoch_dig4
      integer*2 icpl_ct
      logical*2 icpl_printed(max_cpl_sites), all_cpl_printed
      character*3 center_abr_use
      character*4 center_label_use
      character*255 center_full_name_use
      character*255 root_dir,finam
      integer*2 root_len
      character*1 cdum
      logical*2 inack,read_loop
      character*255 cbuf1,cbuf2,contact_info
      character*80 key_field,software_value,hardware_value
      character*10 release_date,revision_date
      character*32 sysname,nodename,hardware
      character*9 full_months_array(12),full_last_month
      integer*4 iuer
      integer*2 lensp
      real*8 full_last_max,full_last
      integer*2 ilast_mon,ilast_day,ilast_year,ilast_cent, &
     &          ilast_year_dig4
      save sinex_descrip
      data sinex_descrip /'none'/
      data full_months_array/'January  ','February ', &
     &                       'March    ','April    ', &
     &                       'May      ','June     ', &
     &                       'July     ','August   ', &
     &                       'September','October  ', &
     &                       'November ','December '/
!
      kdebug = .false.
      sta_all_disp = 'all_the_same'
      lensp = trimlen(spool_name)
!
      if (do_sinex_non) then
!       Get values needed for some sinex output sections.
!       solve.i parameters
        center_abr_use = CENTER_ABR
        center_label_use = CENTER_LABEL
        center_full_name_use = CENTER_FULL_NAME
!       path to root directory of program's source and executable
        call get_gsnoop_envs(root_dir,root_len )
      endif
!
      do ict = 1,max_cpl_sites
        icpl_printed(ict) = .false.
      enddo
!
      i_jan = 1
      i_one = 1
!
      if (do_annual) then
        write (qstr , "('Writing the annual positions table')")
        call asnl(qstr )
      end if
      if (do_iers_site) then
        write (qstr , "('Writing the iers site positions table')")
        call asnl(qstr )
      end if
      if (do_sinex_non) then
        write (qstr , "('Writing the sinex non-covariance tables')")
        call asnl(qstr )
      end if
      if (do_sinex_cov) then
        write (qstr , "('Writing the sinex covariance-based ', &
     &                  'non-apriori tables')")
        call asnl(qstr )
      end if
      if (do_sinex_cap) then
        write (qstr , "('Writing the sinex covariance-based ', &
     &                  'apriori tables')")
        call asnl(qstr )
      end if
      if (do_3_sig) then
        write (qstr , "('With 3-sigma errors')")
        call asnl(qstr )
      end if
!
!     If annual positions were not found in the spool file, use the
!     default site reference epoch.
!
      if (epoch_count .eq. 0) call default_epoch
!
!     open list of stations with monument type code.
      open(49, iostat=ios, file=SOLVE_SAVE_DIR//'antenna.dat', &
     &         status='old')
!
      if (do_3_sig) Write(41,"('Errors in this table are 3-sigma')")
      if (do_3_sig) Write(48,"('C Errors in this table are 3-sigma')")
      if (do_3_sig) Write(70,"('C Errors in this table are 3-sigma')")
      if (do_3_sig) Write(71,"('C Errors in this table are 3-sigma')")
!
      Write(qstr ,'("Writing annual positions table.")')
      call asnl(qstr )
!
      Do J=1,epoch_count
        call newcents(epochs(j),epoch_dig4 )
        Write(qstr ,'("  Doing ",I4)') epoch_dig4
        call asnl(qstr )
!
!       Convert the reference epoch to the julian date format (minus
!       the base of 2400000 assumed to be common to all dates).
!
        rrepoch = fjldy(i_jan,i_one,epochs(j))
        rrepoch = rrepoch - 2400000.0D0
        irepoch = jidint(rrepoch)
        if (do_iers_site) then
!
!         A couple of files will need headers that give the month and year of
!         the last arc.  Calculate this now.   (The same information will be
!         calculated later in full_stop, but it's easier to just duplicate it
!         here.
!
          full_last_max = 0.0D0
          Do I=1,nsite
            Read(SITE_NAMES(I)(19:23),'(I5)') msite
            full_last = sta_mjdmax(msite)  + 2400000.0D0
            if (full_last.gt.full_last_max)  full_last_max = full_last
          enddo
          call mdyjl(ilast_mon,ilast_day,ilast_year,ilast_cent, &
     &               full_last_max )
          call newcents(ilast_year,ilast_year_dig4 )
          full_last_month = full_months_array(ilast_mon)
        endif
!
        if (do_iers_site.or.do_sinex_non) then  !write headers
          Write &
     &    (48, &
     &    '("C TRF solution using data through ",A," ",I4)')full_last_month(1:trimlen(full_last_month)), &
     &    ilast_year_dig4
          Write &
     &    (48,'("C Using spoolfile:  ",A)') spool_name(1:lensp)
          Write &
     &    (48,'("C",/,"C Notes:")')
          Write &
     &    (48,'("C   Sigmas are formal errors only.  ", &
     &          "A floor of 3 mm for reference")')
          Write &
     &    (48,'("C frame definition should be RSSed.")')
          Write &
     &    (48,'("C   Stations with two lines (marked by pq) have ", &
     &          "discontinuous positions before")')
          Write &
     &    (48,'("C and after the epoch at the end of the line ", &
     &          "because of earthquakes or changes")')
          Write &
     &    (48,'("C in mechanical structure.")')
          Write &
     &    (48,'("C   Not all stations have velocities measured by ", &
     &          "VLBI.  For those stations")')
          Write &
     &    (48,'("C without VLBI velocities, the epoch position and ", &
     &          "sigma are derived from")')
          Write &
     &    (48,'("C VLBI position and NUVEL1-A velocities with nominal ", &
     &          "horizontal velocity")')
          Write &
     &    (48,'("C error of 3 mm/yr and zero vertical error.")')
          Write &
     &    (48,'("C   The vertical position of stations is generally ", &
     &          "2-3 times more uncertain")')
          Write &
     &    (48,'("C than the horizontal.")')
          Write &
     &    (48, &
     &      '("C",/,"C VLBI Site positions at January 1.5, ",I4,/)')epoch_dig4
          Write(48,'( &
     & "C    DOMES    NAME           CDP       X           &
     & Y            Z        Sig   Sig   Sig  Ref   Cent Span"/, &
     & "C                            Mon      (m)          (m)          &
     &(m)        X     Y     Z  Epoch Epoch Days")')
        end if
!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
        if (do_iers_site) then  !write headers
          write(70,'( &
     &     "C TRF solution using data through ",A," ",I4)') &
     &      full_last_month(1:trimlen(full_last_month)), &
     &      ilast_year_dig4
          write(70,'( &
     &     "C Using spoolfile:  ",A)') spool_name(1:lensp)
          write(70,'( &
     &     "C",/," ")')
          Write (70,'("C",/,"C VLBI Site velocities")')
          Write(70,'( &
     &"C DOMES        CDP           CDP    X-Vel   Y-Vel   Z-Vel    X   &
     &   Y      Z   Cent  Span",/, &
     &"C  SITE       NAME           MON   (m/yr)  (m/yr)  (m/yr)   Sig&
     &    Sig    Sig  Epoch days")')
!
          Write &
     &    (71, &
     &    '("C",/,"C VLBI Site positions at January 1.5, ",I4)')epoch_dig4
          Write(71,'( &
     & "C    DOMES    NAME           CDP       X            Y           &
     & Z        Sig   Sig   Sig  Ref   Cent Span"/, &
     & "C                            Mon      (m)          (m)          &
     &(m)        X     Y     Z  Epoch Epoch  MJD")')
!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
          Write (71,'("C",/,"C VLBI Site velocities")')
          Write(71,'( &
     &"C DOMES        CDP           CDP    X-Vel   Y-Vel   Z-Vel    X   &
     &   Y      Z   Cent  Span",/, &
     &"C  SITE       NAME           MON   (m/yr)  (m/yr)  (m/yr)   Sig &
     &   Sig    Sig  Epoch days",/"C")')
!
        end if
!
        if (do_annual) then   !write headers
          Write &
     &    (41, &
     &    '(//,20x," VLBI Site positions at January 1.5, ",I4,/)')epoch_dig4
          Write(41,'("  Site   Mon.", &
     &          14x ,"X",6x,"Error", &
     &          13x ,"Y",6x,"Error", &
     &          13x ,"Z",6x,"Error",/, &
     &          25x ,"(mm)",7x,"(mm) ", &
     &          11x ,"(mm)",7x,"(mm) ", &
     &          11x ,"(mm)",7x,"(mm)",/)')
        end if
!
        if (do_sinex_non) then
          write(85,"('+SOLUTION/EPOCHS')")
          write(85,"('*CODE PT SOLN T DATA_START__ ', &
     &        'DATA_END____ MEAN_EPOCH__')")
        end if
!       Some initialization
        if (do_sinex_non) then
          full_start_min = 1000000000.0D0
          full_stop_max = 0.0D0
        endif
!
!
        Do I=1,nsite
!
          Read(SITE_NAMES(I)(19:23),'(I5)') msite
          If(xyz_epoch_sig(1,J,msite).lt. .0001) then !reference site
            Warning = 'rf'
          else !Not the reference site
            if (SITE_NAMES(I)(13:18) .ne. "000000") then
              qepoch = SITE_NAMES(I)(13:18)
              Warning = "pq"
            else
              qepoch = "      "
              Warning = "  "
            end if
          Endif
!
          if (do_iers_site.or.do_sinex_non)  then
!
!           Read though the stations list to get the monument type,
!
            kdone = .false.
            do while (.not. kdone)
              read(49,'(a)', end=100) cbuf
              if (cbuf(1:1) .ne. "#") then  !skip comments
!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
                if ((cbuf(1:8)   .eq. site_names(i)(1:8))  .and. &
     &              (cbuf(10:13) .eq. site_names(i)(9:12))) then
                  qtype  = cbuf(22:22)
                  qdomes = cbuf(17:25)
                  kdone = .true.
                  if (site_names(i)(13:18) .ne. "000000") then
!                                    !next entry should be same name and
!                                    !mon but different ep date and domes
                    read(49,'(a)', end=100) cbuf
                    if ((cbuf(1:8)   .eq. site_names(i)(1:8))  .and. &
     &                  (cbuf(10:13) .eq. site_names(i)(9:12))) then
                      qtype  = cbuf(22:22)
                      qdomes = cbuf(17:25)
                      kdone = .true.
                    end if
                  end if
                end if
              end if  !not # comment
            end do  !whew!
!
 100        if (kdone .eq. .false.) then  !station not in sta_list
              qtype = "?"
              write (qstr , &
     &                        '("WARNING Station ",a," not found in: ")')site_names(i)(1:12)
              call asnl(qstr )
              write (qstr , &
     &           '("        ",A,"antenna.dat")')SOLVE_SAVE_DIR
              call asnl(qstr )
              write (qstr , '("<RETURN> to continue")')
              call asnl(qstr )
            end if
!
!
! 1001       format (
!     .       1X, a4, 2x, a1, " 0 ", a8, a2,
!     .       1x, 3f13.3, 1x,
!     .       2f7.1, 5x, 3f6.3, 2x, a6)
!
 1001       format &
     &       ("X", 1X, a9, 1X, &
     &       a8,  1X, a2, 4X, 1X, a4, &
     &       3f13.3,   3f6.3, 1X, &
     &       I5,  1X, I5, 1X, I4, &
     &       1X,  a6)
!
!
!
            if (do_iers_site.or.do_sinex_non) then
              write(48, &
     &        1001)qdomes, &
     &        SITE_NAMES(I)(1:8), &
     &        Warning, &
     &        SITE_NAMES(I)(9:12), &     !CDP monument
     &        (xyz_epoch    (m,J,msite)/1.0d3,m=1,3), &   !convert from mm to m
     &        (xyz_epoch_sig(m,J,msite)*scale/1.0d3,m=1,3), &
     &        irepoch, &
     &        jidint(sta_mean_epoch(msite)), &
     &        jidint(sta_span(msite)), &
     &        qepoch
            end if
!
            if (do_sinex_non) then
!
!             Write solution epochs file for 1995 iers site
!             submission for this site (and any cpl site that should be
!             inserted at this point).
!
!             First check the cpl site name array to see if it's time to
!             print any cpl sites.  (The loop will print a cpl site immediately
!             before  printing the first site that has a lexically greater name.
!             (e.g., to insert HRAS 085 between HOHNBERG and JPL MV1,
!                   pass n-1 will print HOHNBERG and
!                   pass n   will print HRAS 085, then JPL MV1.)
!             This algorithm assumes that the cpl site name array is sorted
!             alphabetically, so that if two or more cpl sites must be inserted
!             at a spot, they will be guaranteed to be inserted in order.)
!
              do icpl_ct = 1,ncplstat
                if ( (lgt(site_names(i)(1:8),qcpl_stat(icpl_ct))) .and. &
     &                        (.not. icpl_printed(icpl_ct))   ) then
                  icpl_printed(icpl_ct) = .true.
                  do mct = 1,cpls_nepochs(icpl_ct)+1
                    if (cpls_max(mct,icpl_ct).gt.0.0D0) then
!
!                     There was data in the interval
!
!                     Convert the epochs to a printable form, truncating the
!                     mean epoch to the nearest minute.
!
                      full_cpls_max = &
     &                  cpls_max(mct,icpl_ct) + 2400000.0D0
                      call &
     &                     epoch_form95(full_cpls_max,cpls_max_disp(mct,icpl_ct) )
                      full_cpls_min = &
     &                  cpls_min(mct,icpl_ct) + 2400000.0D0
                      call &
     &                     epoch_form95(full_cpls_min,cpls_min_disp(mct,icpl_ct) )
                      full_cpls_mean = &
     &                  cpls_mean(mct,icpl_ct) + 2400000.0D0
                      call &
     &                     epoch_form95(full_cpls_mean,cpls_mean_disp )
!
                      read(cpls_mean_disp(8:12),"(i5)") jmin_trunc
                      jmin_trunc = jmin_trunc / 60 * 60
                      write(cpls_mean_disp(8:12),"(i5.5)") jmin_trunc
                      write(85,"( &
     &                 1x,a4,2X,'A',3X,I2,1X,'R',1X,A12,1X,A12,1X,A12)") &
     &                  qmcpl(icpl_ct),mct, &
     &                  cpls_min_disp(mct,icpl_ct), &
     &                  cpls_max_disp(mct,icpl_ct), &
     &                  cpls_mean_disp
                    else
!                     No data in the interval.  Print a placeholder
                      write(85,"( &
     &                   1x,a4,2X,'A',3X,I2,1X,'R',1X,'00:000:00000',1X, &
     &                     '00:000:00000',1X,'00:000:00000')") &
     &                  qmcpl(icpl_ct),mct
                      cpls_min_disp(mct,icpl_ct) = '00:000:00000'
                      cpls_max_disp(mct,icpl_ct) = '00:000:00000'
                    endif
                  enddo
                endif
              enddo
!
!             Now print the information for the current non-cpl site.
!
              if (site_names(i)(13:18).eq."000000") then
                isoln = 1
              else
                isoln = 2
              end if
              isolns(i) = isoln
              full_start = sta_mjdmin(msite) + 2400000.0D0
              call epoch_form95(full_start,sta_min_disp )
              sta_min_disps(i) = sta_min_disp
!             See if all of the starting and ending epochs for the sites
!             (i.e., the epochs of the sites' first and last appearances
!             in the solution) are the same.  If so, assume this is a one-day
!             arc solution and print this date in the solest and
!             solestap files.
              if (i.eq.1) then
                sta_first_disp = sta_min_disp
              else
                if (sta_min_disp.ne. &
     &            sta_first_disp)sta_all_disp = '_individual_'
              endif
              full_stop = sta_mjdmax(msite)  + 2400000.0D0
              call epoch_form95(full_stop,sta_max_disp )
              sta_max_disps(i) = sta_max_disp
              if (sta_max_disp.ne. &
     &            sta_first_disp)sta_all_disp = '_individual_'
              full_mean = sta_mean_epoch(msite) + 2400000.0D0
              call epoch_form95(full_mean,sta_mean_disp )
              if (full_start.lt.full_start_min) then
                full_start_min = full_start
                sta_min_disp_min = sta_min_disp
              endif
              if (full_stop.gt.full_stop_max) then
                full_stop_max = full_stop
                sta_max_disp_max = sta_max_disp
              endif
!             Truncate the mean epoch to the nearest minute.
              read(sta_mean_disp(8:12),"(i5)") jmin_trunc
              jmin_trunc = jmin_trunc / 60 * 60
              write(sta_mean_disp(8:12),"(i5.5)") jmin_trunc
              write(85,"( &
     &          1x,a4,2X,'A',4X,I1,1X,'R',1X,A12,1X,A12,1X,A12)") &
     &          site_names(i)(9:12),isoln, &
     &          sta_min_disp, sta_max_disp, sta_mean_disp
            end if
!
            if (do_iers_site) then
              write(71, &
     &        1001)qdomes, &
     &        SITE_NAMES(I)(1:8), &
     &        Warning, &
     &        SITE_NAMES(I)(9:12), &     !CDP monument
     &        (xyz_epoch    (m,J,msite)/1.0d3,m=1,3), &   !convert from mm to m
     &        (xyz_epoch_sig(m,J,msite)*scale/1.0d3,m=1,3), &
     &        irepoch, &
     &        jidint(sta_mean_epoch(msite)), &
     &        jidint(sta_span(msite)), &
     &        qepoch
!
 1002         format &
     &         ("V", 1X, a5, 5x, &
     &         a8,  8X, a4, 1X, &
     &         3f8.4,    3f7.4, 1X, &
     &         I5, 1X, I4)
!
              if (warning .ne. 'pq') then
                write(71, &
     &          1002)qdomes(1:5), &
     &          SITE_NAMES(I)(1:8), &
     &          SITE_NAMES(I)(9:12), &     !CDP monument
     &          (xyz_dot    (m,msite)/1.0d3,m=1,3), &  !convert from mm/yr to m/yr
     &          (xyz_dot_sig(m,msite)*scale/1.0d3,m=1,3), &
     &          jidint(sta_mean_epoch(msite)), &
     &          jidint(sta_span(msite))
!
                write(70, &
     &          1002)qdomes(1:5), &
     &          SITE_NAMES(I)(1:8), &
     &          SITE_NAMES(I)(9:12), &     !CDP monument
     &          (xyz_dot    (m,msite)/1.0d3,m=1,3), &  !convert from mm/yr to m/yr
     &          (xyz_dot_sig(m,msite)*scale/1.0d3,m=1,3), &
     &          jidint(sta_mean_epoch(msite)), &
     &          jidint(sta_span(msite))
              end if
            end if
            rewind(49)
          end if
!
          if &
     &      (do_annual)Write(41, &
     &      '(A8,1X,A4,a2,3(F17.1,F8.1),2x,A6)')SITE_NAMES(I)(1:8), &
     &      SITE_NAMES(I)(9:12), &
     &      Warning, &
     &      (xyz_epoch    (m,J,msite), &
     &       xyz_epoch_sig(m,J,msite)*scale,m=1,3), &
     &      qepoch
!
        Enddo
      Enddo
      close(49)
!
      if (sta_all_disp.eq. &
     &    'all_the_same')sta_all_disp = sta_first_disp
!
!     Cheat a little here.  There is -- and probably always will be -- only
!     one cpl site, HRAS 085.  And if any are ever added, the preceding logic
!     will print them to the iers_solep_<> file.  But there is an exception:
!     a cpl site at the end of
!     the site list, alphabetically.  This is unlikely, because YUMA is the
!     last site.  However, in case a site is ever added, warn the user that
!     the subroutine must be updated.
!
      all_cpl_printed = .true.
      do icpl_ct = 1,ncplstat
        if (.not.icpl_printed(icpl_ct)) all_cpl_printed = .false.
      enddo
      if (.not. all_cpl_printed) then
        write(qstr,"('Error: some cpl sites were skipped. ')")
        call asnl(qstr )
        write(qstr,"( &
     &     'Probably they are at the end of the site list, ')")
        call asnl(qstr )
        write(qstr,"('which Gsnoop is not coded to handle.')")
        call asnl(qstr )
        write(qstr,"('Please tell a Gsnoop programmer.')")
        call asnl(qstr )
        write(qstr,"('(Type any key to acknowlege)')")
        call asnl(qstr )
        call getstr_f(qstr )
      endif
!
      if (do_sinex_non) write(85,"('-SOLUTION/EPOCHS')")
!
!     Produce the site/id section of the 1995 iers site submission
!
      close(48)
      if (do_sinex_non) call site_id_sect(user_path,file_tag, &
     &        sta_min_disps,sta_max_disps, &
     &        qcpl_stat,qmcpl,ncplstat,cpls_nepochs, &
     &        cpls_min_disp,cpls_max_disp,isolns)
!
!     Optionally sort the iers_siteid and iers_solep files by longitude.
!
      if (do_sinex_non) then
        do while (sort_choice.ne.'L'.and.sort_choice.ne.'A')
          write (qstr , &
     &      "('Sort the iers_siteid and iers_solep files: ')")
          call asnl(qstr )
          write (qstr , &
     &      "('   by longitude (L) ')")
          call asnl(qstr )
          write (qstr , &
     &      "('   alphabetically (A)?')")
          call asnl(qstr )
          call getstr_f(sort_choice )
          call casefold(sort_choice )
        enddo
!       The file starts out sorted alphabetically, so just go on if this
!       has been selected.
        close(85)
        close(86)
        if (sort_choice.eq.'L') then
          call sort_snx_non(user_path,file_tag,sort_choice )
        endif
      endif
!
!     Produce the solution/statistics section of the 1996 iers site submission
!
      if (do_sinex_non) call solstat_sect
!
!     Produce the solution/estimate and/or solution/matrix estimate L cova
!     files (as either non-apriori or apriori files, whichever was
!     requested).
!
      if (do_sinex_cov.or.do_sinex_cap) then
        if (do_sinex_cov) then
          ap_choice = .false.
        else
          ap_choice = .true.
        end if
        ivlen = trimlen(cov_name)
        iclen = trimlen(control_name)
        call id_cov_type(control_name,iclen,covs_wanted,cons_override )
        call solest(cov_name,ivlen,control_name,iclen, &
     &                ap_choice,ref_ep_yr,ref_ep_mn,ref_ep_dy, &
     &                covs_wanted,cons_override,sta_all_disp, &
     &                ncplstat,qcpl_stat )
        call solmat(cov_name,ivlen,ap_choice,covs_wanted, &
     &              covs_found,ncovparms )
      end if
!     Produce the sinex header line and sinex input/history sections.
      if (do_sinex_non.and.(do_sinex_cov.or. &
     &     do_sinex_cap))call snx_head_hst(sta_min_disp_min,sta_max_disp_max, &
     &        ncovparms,cons_override,covs_found,file_create_time, &
     &        center_abr_use,center_full_name_use)
!     Produce the sinex input/files section
      if (do_sinex_non) then
        if (sinex_descrip.eq.'none') then
          qstr = 'Give me a description for your solution(s): '
          call asnl(qstr )
          qstr = ' (maximum of 30 characters, please)'
          call asnl(qstr )
          qstr = '123456789012345678901234567890'
          call asnl(qstr )
          call getstr_f(sinex_descrip )
        endif
        call path_break(spool_name,spool_directory,isp_dlen, &
     &                  spool_base,isp_blen )
!       Generate sinex_input_files_<> file.
        write(106,"('+INPUT/FILES')")
        write(106, &
     &    "('*Note: ',A,' = ',A)")center_abr_use, &
     &    center_full_name_use(1:trimlen(center_full_name_use))
        write(106,"('*Own __Creation__ ', &
     &    '__filename___________________ ', &
     &    '__Description ________________')")
        write(106, &
     &    "(1X,A,1X,A12,1X,A14,16X,A30)")center_abr_use, &
     &    file_create_time,spool_base(1:14),sinex_descrip
        write(106,"('-INPUT/FILES')")
!       Generate file_comment_all file.
        write(109,"('+FILE/COMMENT')")
        write(109,"('-FILE/COMMENT')")
!       Generate gps_phase_cent_all file.
        write(111,"('+SITE/GPS_PHASE_CENTER')")
        write(111,"('*                           ', &
     &    'UP____ NORTH_ EAST__ UP____ NORTH_ EAST__')")
        write(111,"('*Receiver type_______ S/N__ ', &
     &    'L1-ARP --> (m)______ L2-ARP --> (m)______  ', &
     &    'Az El Model')")
        write(111,"(' ----VLBI Station---- --NM-  ', &
     &    '.0000  .0000  .0000  .0000  .0000  .0000  ----')")
        write(111,"('-SITE/GPS_PHASE_CENTER')")
!       Generate the input_ack_all file which consists of lines taken
!         directly from the '<center>.hea file.
!       (At the same time, save off info for the file_ref_all_<> file.)
        finam = root_dir(1:root_len)//'local/'//CENTER_LABEL//'.hea'
        OPEN (49, FILE=FINAM(1:trimlen(finam)), &
     &      IOSTAT=IOS,ERR=550,STATUS='OLD', &
     &      FORM='FORMATTED',ACCESS='SEQUENTIAL')
 550    IF ( IOS .NE. 0 ) THEN
          write (qstr , &
     &      "('Error ',i5,' opening header file: ')")ios
          call asnl(qstr )
          write (qstr, &
     &      "(A)")finam(1:trimlen(finam))
          call asnl(qstr )
          write (qstr , "('Refer to CALC/SOLVE installation ', &
     &      'documentation.')")
          call asnl(qstr )
          write (qstr , "('Output SINEX files will be incomplete. ')")
          call asnl(qstr )
          write (qstr,"('Hit return to acknowledge.')")
          call asnl(qstr )
          call getstr_f(cdum )
        END IF
        inack = .false.
        read_loop = .true.
        do while(read_loop)
          read(49,"(A)",iostat=ios,err=555,end=560) cbuf2
 555      if (ios.ne.0) then
            write (qstr , "('Error ',i5,' reading header file: ')") ios
            call asnl(qstr )
            write (qstr, &
     &        "(A)")finam(1:trimlen(finam))
            call asnl(qstr )
            write (qstr , "('Output sinex files will be incomplete. ')")
            call asnl(qstr )
            write (qstr,"('Hit return to acknowledge.')")
            call asnl(qstr )
            call getstr_f(cdum )
          endif
          cbuf1 = cbuf2
          call splitstring(cbuf2,key_field,cbuf2 )
!         Save info for file_ref_all_<>.
          if (key_field.eq. &
     &      'CONTACT')contact_info = cbuf1
!         Write relevant lines directly to input_ack_all_<>.
          if (key_field.eq.'+INPUT/ACKNOWLEDGEMENTS') inack = .true.
          if (inack) write(112,"(A)") cbuf1(1:trimlen(cbuf1))
          if (key_field.eq.'-INPUT/ACKNOWLEDGEMENTS') inack = .false.
        enddo
 560    close(49)
!       Now generate file_ref_all file, which needed info from <center>.hea.
!       But first get a little more information.
        IUER = -1
        CALL GET_SOLVE_VERSION ( RELEASE_DATE, REVISION_DATE, IUER )
        CALL GETINFO_SYSTEM ( SYSNAME, NODENAME, HARDWARE )
        software_value = "CALC/SOLVE  Revision_date: "//revision_date
        hardware_value = 'HP'//HARDWARE(1:trimlen(HARDWARE))//'   '// &
     &          SYSNAME(1:trimlen(SYSNAME))
!       Finally, generate the file
        write(110,"('+FILE/REFERENCE')")
        write(110, &
     &    "(' DESCRIPTION        ',A,' TRF solution')")center_full_name_use(1:trimlen(center_full_name_use))
        write(110,"(' OUTPUT             VLBI Sinex File')")
        write(110, &
     &    "(A)")contact_info(1:trimlen(contact_info))
        write(110, &
     &    "(' SOFTWARE           ',A)")software_value(1:trimlen(software_value))
        write(110, &
     &    "(' HARDWARE           ',A)")hardware_value(1:trimlen(hardware_value))
        write(110, &
     &     "(' INPUT              ',A,' VLBI Solve superfiles')")center_abr_use
        write(110,"('-FILE/REFERENCE')")
!       Last of all, generate sinex_trailer file.
        write(113,"('%ENDSNX')")
      endif
!
      Return
      END
