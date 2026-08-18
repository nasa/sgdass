!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
      SUBROUTINE open_files(comment, path, ipath, file_id)
!
      implicit none
!
!
      INCLUDE 'gsnoop_com.i'
      integer*2     ipath, lid
      integer*4     ios
      integer*2     trimlen, lencom, lensp
      integer*2     lencov, lencon
      character*80  qstr
      character*(*) file_id
      character*(*) comment, path
!
!     LUs  40 = spoolfile
!          41 = annual_positions file
!          42 = correlations file
!          43 = global epoch position, velocity, and correlation file.
!          44 = horizontal velocity and error ellipse file
!          45 = hypermap input file - site velocity vectors and error ellipses
!          46 = hypermap input file - site velocity info aprioris.
!          47 = iers source positions file
!          48 = iers site positions file
!          49 = sitename file
!          50 = site positions flyby file
!          51 = source flyby file
!          52 = uen adjustments file
!          53 = uen sigma bins
!          54 = xyz positions for episodic sites.
!          55 = uen adjustments for episodic sites.
!          56 = latitude & longitude file
!          57 = configuration file
!          58 = continuous piecewise linear xyz file
!          59 = continuous piecewise linear uen file
!          60 = reserved
!          62 = reserved
!          63 = site positions flyby file
!          64 = Spool file list
!          65 = hypermap horizontal adjustment info
!          66 = gmt velocity file for psvelomeca
!          67 = gmt positions file for psxy
!          68 = up velocities file
!          69 = gmt model velocities file for psvelomeca
!          70 = iers site velocities file
!          71 = iers site positions and velocities file
!          72 = gmt velcities diff (model - observed) file
!          73 = gmt velcities 360 file
!          74 = gmt velcities diff 360 (model - observed) file
!          75 = cmpar positions file
!          76 = cmpar xyz velocities file
!          77 = cmpar uen velocities file
!          78 = luerr in open_spool_list
!          79 = min sig files
!          80 = gmt model velocities file (0-360)
!          81 = src plot files  (0-360) and (360 -0) in RA
!          82 = gmt velcities adj (observed - model) file
!          83 =
!          84 = gmt velcities adj 360 (observed - model) file
!          85 = 1995 iers site submission file (solution epoch section)
!          86 = 1995 iers site submission file (site id section)
!          (87 = used to read input files for the duration of subroutines)
!          88 = 1995 iers site submission file (solution estimate section)
!          89 = 1995 iers iste submission file (solution/matrix_estimate l cova
!                  section)
!          90 = 1995 iers site submission file (solution/apriori section)
!          91 = 1995 iers site submission file (solution/matrix_apriori l cova)
!          92 = control file (used for solution estimate section)
!          93 = special file of sites used for solution/estimate and
!               solution/apriori sections
!          94 = file with latitude, longitude, rss of cartesian errors,
!                 errors themselves and site names.
!          95 = special minimum rss file for gmt mapping:
!                 has one line per site, with
!                  longitude and latitude and rss of the
!                   minimum x,y,z position sigmas
!          96 = up adjustment file for gmt plotting
!          98 = file of uen sigmas, plus rss's of the horizontal sigmas
!               and of all three sigmas.  Done for positions and velocities
!               for a total of 10 values.
!          99 = 1996 iers site submission file (solution statistics section)
!         100 = command file for "batch processing"
!         101 = sinex header file
!         102 = input/history file
!         103 = sinex site/receiver file
!         104 = sinex site/antenna file
!         105 = sinex site/eccentricity file
!         106 = sinex input/files file
!         107 = reserved (used in get_file_gen to check file existence)
!         108 = "horaz_" (horizontal/azimuth values with longitude/latitude,
!                    created for  DGG's 12/98 AGU).
!         109 = file_comment_all_<>
!         110 = file_ref_all_<>
!         111 = gps_phase_cent_all_<>
!         112 = input_ack_all_<>
!         113 = sinex_trailer_<>
!         114 = gsnoop_header_<> (comment and input spoolfile (and input
!                                  control and covariance files,
!                                  if appropriate), replacing this information
!                                  being put at the top of the output files)
!         115 = source_basic_stats_<> basic source statistics (adjustments,
!                                     uncertainties, ra/dec correlation)
!
!     Added hypermap adjustments file and improved error trapping.
!     DS Caprette Hughes STX 93/03/05
!
!     Removed excessive blank lines from headers.  Used trimlen to
!     shorten headers to last nonblank char.
!     DS Caprette Hughes STX 93/08/10
!
!     Ability to print the 6 output files for the 1995 iers site submission.
!     K. Baver 95/04/07
!
!     Note usage of new lu (93) for setting of constraint levels in the
!     1995 iers site submission solution/estimate and solution/apriori
!     files.  K. Baver 4/19/95
!
!     Add two new files, the "rss" file associated with lu 94
!     and the minimum rss file associated with lu 95.
!       K. Baver 6/27/95
!
!     Add new file, up adjustment file for gmt plotting
!      (associated with lu 96). K. Baver 9/25/95
!
!     Revive old, mysteriously disabled file, lu 65, as "horiz_adj_<>".
!       K. Baver 10/16/95
!
!     Add new file, lu 98, as "uen_sigs_<>".
!       K. Baver 11/20/95
!
!     Add new file, lu 99, as "iers_solstat_<>".
!       K. Baver  4/24/96
!
!     On the second line of the sinex covariance output files,
!     print the covariance and control files used instead of the unused
!     spoolfile.
!       K. Baver  4/25/96
!
!     Documentation (note lu 100).
!       K. Baver  7/11/96
!
!     Add new files, lus 101 and 102, as "sinex_header_<>"
!     and "sinex_inhist_<>".
!       K. Baver  7/18/96
!
!     Add new files, lus 103-105, as "sinex_site_recvr_<>",
!     "sinex_site_ant_<>" and "sinex_site_ecc_<>".
!       K. Baver  7/19/96
!
!     Add new file, lu 106, as "sinex_input_files_<>", K. Baver 7/22/96
!
!     Rename the "360" files so that the 360 designation is placed on the
!     end, for compatibility with the current plotting script.  K. Baver 8/1/96
!
!     Fix a few bugs which reported the wrong input item files in certain
!        IERS SINEX output files.   K. Baver 4/16/97.
!
!     Note usage of lu 107 elsewhere.  K. Baver 8/3/98.
!
!     Add new file, lu 108, as "horaz_".  K. Baver 12/2/98.
!
!     Fix bug: only open cpls_uen_<> file if a piecewise linear station
!       has been selected. (Otherwise, if this is opened when position page
!       option 2 is chosen, it will produce a useless file with just a header.)
!       K. Baver 9/5/00.
!
!     Gsnoop will now write file_comment_all_<>, file_ref_all_<>,
!       gps_phase_cent_all_<>, input_ack_all_<> and sinex_trailer_<> files.
!       K. Baver 11/1/00.
!
!     Now can write header information (comment and input spool (and control
!       and covariance) file) to
!       a separate header file instead of to the tops of all output files.
!       K. Baver 9/14/01.
!
!     Add new file, source_basic_stats_<>.
!       K. Baver 3/24/03.
!
!
          lencom = trimlen (comment)
          lensp = trimlen (spool_name)
          lencov = trimlen (cov_name)
          lencon = trimlen (control_name)
          lid = trimlen (file_id)
!
      if (separate_header_file) then
        OPEN(114,IOSTAT=IOS,FILE=path(1:ipath)//'gsnoop_header_'// &
     &                File_id(1:lid),STATUS='UNKNOWN')
        WRITE(qstr,'("IOS opening of gsnoop header file is",I5)')IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          Write(114,'(a)') comment(1:lencom)
          Write(114,'("Using spool file: ",a,$)') spool_name(1:lensp)
          if (control_name(1:4).ne.'none') &
     &     Write(114,'("  control file:  ",a,$)') control_name(1:lencon)
          if (cov_name(1:4).ne.'none') &
     &     Write(114,'("  covariance file:  ",a,$)') cov_name(1:lencov)
          write(114,'(" ")')
          CLOSE(114)
        end if
      end if
!
!     Open the site lat lon file
      if (do_lat_lon) then
        OPEN(56,IOSTAT=IOS,FILE=path(1:ipath)//'sitell_'// &
     &               File_id(1:lid),STATUS='UNKNOWN')
        WRITE(qstr,'("IOS opening of site lat lon file is",I5)')IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(56,'(a)') comment(1:lencom)
            Write(56,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          endif
        end if
      end if
!
!     Open the uen sigma bins file
      if (bin_uen_sigs) then
        OPEN(53,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'uen_bin_'//File_id(1:lid),STATUS='UNKNOWN')
        WRITE(qstr,'("IOS opening of uen sigma bins file is",I5)')IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(53,'(a)') comment(1:lencom)
            Write(53,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          endif
        end if
        OPEN(98,IOSTAT=IOS, &
     &  FILE=path(1:ipath)// &
     &    'uen_sigs_'//File_id(1:lid),STATUS='UNKNOWN')
        WRITE(qstr,'("IOS opening of uen sigmas file is",I5)')IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(98,'(a)') comment(1:lencom)
            Write(98,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
      end if
!
!     Open the episodic site positions file
      if (do_ep_sites) then
        OPEN(54,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'ep_xyz_'//File_id(1:lid),STATUS='UNKNOWN')
        WRITE(qstr,'("IOS open of episodic site positions file is",I5)') &
     &  IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(54,'(a)') comment(1:lencom)
            Write(54,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
        OPEN(55,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'ep_xyz_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS open of episodic site positions file is",I5)') &
     &   IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(55,'(a)') comment(1:lencom)
            Write(55,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
      end if
!
!     Open the annual position file
      if (do_annual) then
        OPEN(41,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'annual_p_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of annual position file is",I5)') &
     &  IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(41,'(a)') comment(1:lencom)
            Write(41,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
      end if
!
!     Open the global epoch position, velocity, and correlation file
!     Open the topocentric adjustments file.
!     & Open the topocentric site velocities file.
      if (do_vel) then
      OPEN(43,IOSTAT=IOS,FILE=path(1:ipath)//'cvel_'//File_id(1:lid), &
     &STATUS='UNKNOWN')
      WRITE(qstr,'("IOS from open of epoch position file is",I5)') IOS
      call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(43,'(a)') comment(1:lencom)
            Write(43,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
      OPEN(44,IOSTAT=IOS,FILE=path(1:ipath)//'tvel_'//File_id(1:lid), &
     &STATUS='UNKNOWN')
      WRITE(qstr,'("IOS from open of site velocities file is",I5)') IOS
      call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(44,'(a)') comment(1:lencom)
            Write(44,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
      OPEN(108,IOSTAT=IOS,FILE=path(1:ipath)//'horaz_'//File_id(1:lid), &
     &STATUS='UNKNOWN')
      WRITE(qstr,'("IOS from open of ", &
     &   "horizontal/azimuth & latitude/longitude file is",I5)') IOS
      call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(108,'(a)') comment(1:lencom)
            Write(108,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
!     OPEN(58,IOSTAT=IOS,FILE=path(1:ipath)//'cpls_xyz_'//File_id(1:lid),
!    .STATUS='UNKNOWN')
!     WRITE(qstr,'("IOS from open of xyz cpl file is",I5)') IOS
!all asnl(qstr)
!     if (ios .ne. 0) then
!all return_to_continue
!     else
!         Write(58,'(a)') comment(1:lencom)
!         Write(58,'("Using spoolfile:  ",a)') spool_name(1:lensp)
!     end if
!
      cpluen_name=path(1:ipath)//'cpls_uen_'//File_id(1:lid)
      if (do_uen_cpl) then
        OPEN(59,IOSTAT=IOS,FILE= cpluen_name, STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of uen cpl file is",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(59,'(a)') comment(1:lencom)
            Write(59,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
      endif
!
!
!
!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
      OPEN(52,IOSTAT=IOS,FILE=path(1:ipath)//'uena_'//File_id(1:lid), &
     &STATUS='UNKNOWN')
      WRITE(qstr,'("IOS from open of uen adjustments file is",I5)') IOS
      call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(52,'(a)') comment(1:lencom)
            Write(52,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
         end if
!
      end if
!
!     Open the hypermap velocity file.
      if (do_hyper) then
!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
        OPEN(45,IOSTAT=IOS,FILE=path(1:ipath)//'hyper_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of hypermap is",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(45,'(a)') comment(1:lencom)
            Write(45,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
!       Open the hypermap velocity apriori file
        OPEN(46,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'hyper_ap_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of hypermap apriori file is",I5)') &
     &  IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(46,'(a)') comment(1:lencom)
            Write(46,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
!       Open the gmt velocity file
!
        OPEN(66,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'gmt_vel_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of gmt velocity file is",I5)') &
     &             IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(66,'(a)') comment(1:lencom)
            Write(66,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
        OPEN(73,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'gmt_vel_'//File_id(1:lid)//'_360', &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of gmt 360 velocity file is",I5)') &
     &             IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(73,'(a)') comment(1:lencom)
            Write(73,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
!       Open the gmt model velocity files
!
      OPEN(69,IOSTAT=IOS, &
     &FILE=path(1:ipath)//'gmt_mod_'//File_id(1:lid),STATUS='UNKNOWN')
      WRITE(qstr,'("IOS from open of gmt model velocities file is",I5)') &
     &       IOS
      call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(69,'(a)') comment(1:lencom)
            Write(69,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
      OPEN(80,IOSTAT=IOS, FILE=path(1:ipath)//'gmt_mod_'// &
     & File_id(1:lid)//'_360', STATUS='UNKNOWN')
      WRITE(qstr,'("IOS from open of gmt model velocities file is",I5)') &
     &       IOS
      call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(80,'(a)') comment(1:lencom)
            Write(80,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
!       Open the gmt positions file
!
        OPEN(67,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'gmt_pos_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of gmt_pos  positions file is",I5)') &
     &             IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(67,'(a)') comment(1:lencom)
            Write(67,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
!
!       Open the gmt velocities difference file
!
!
        OPEN(72,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'gmt_dif_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of gmt_dif velocities file is",I5)') &
     &             IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(72,'(a)') comment(1:lencom)
            Write(72,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
        OPEN(74,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'gmt_dif_'//File_id(1:lid)//'_360', &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of gmt_dif velocities file is",I5)') &
     &             IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(74,'(a)') comment(1:lencom)
            Write(74,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
!
        OPEN(82,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'gmt_adj_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of gmt_adj velocities file is",I5)') &
     &             IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(82,'(a)') comment(1:lencom)
            Write(82,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
        OPEN(84,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'gmt_adj_'//File_id(1:lid)//'_360', &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of gmt_adj velocities file is",I5)') &
     &             IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(84,'(a)') comment(1:lencom)
            Write(84,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
!       Open the up velocities file
!       Open the up velocities file
        OPEN(68,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'idl_upv_'//File_id(1:lid),STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of up velocities file is",I5)') &
     &             IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(68,'(a)') comment(1:lencom)
            Write(68,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
!       Open the rss file.
!
        OPEN(94,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'gmt_rss_'//File_id(1:lid),STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of gmt rss file is",I5)') &
     &             IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(94,'(a)') comment(1:lencom)
            Write(94,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
!       Open the gmt minimum rss file.
!
        OPEN(95,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'gmt_minrss_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of gmt min rss file is",I5)') &
     &             IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(95,'(a)') comment(1:lencom)
            Write(95,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
!       Open the up adjustment file for gmt plotting
!
        OPEN(96,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'gmt_upadj_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of gmt up adj file is",I5)') &
     &             IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(96,'(a)') comment(1:lencom)
            Write(96,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
!       Open the horizontal adjustments file
!
        OPEN(65,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'horiz_adj_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of horizontal adj  file is",I5)') &
     &             IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(65,'(a)') comment(1:lencom)
            Write(65,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
      end if
!
!     Open the source positions file.
      if (do_iers_source) then
        OPEN(47,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'iers_src_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of iers source file is",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(47,'(a)') comment(1:lencom)
            Write(47,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
      OPEN(81,IOSTAT=IOS, FILE=path(1:ipath)//'src_plt_'// &
     & File_id(1:lid), STATUS='UNKNOWN')
      WRITE(qstr,'("IOS from open of source plot file is",I5)') &
     &       IOS
      call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(81,'(a)') comment(1:lencom)
            Write(81,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
!
!       Open the file of basic source statistics.
!
        OPEN(115,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'source_basic_stats_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of source basic stats file is",I5)') &
     &             IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(115,'(a)') comment(1:lencom)
            Write(115,'("Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
      end if
!
!       Open the IERS annual position file
!       (note: also needed for one of the 1995 style iers site submission
!        files, the site/id file)
!
      if (do_iers_site.or.do_sinex_non) then
        OPEN(48,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'iers_sit_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS open of iers site position file is",I5)')IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(48,'("C ",a)') comment(1:lencom)
            Write(48,'("C Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
      end if
      if (do_iers_site) then
        OPEN(70,IOSTAT=IOS, &
     &   FILE=path(1:ipath)//'iers_vel_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS open of iers site position file is",I5)')IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(70,'("C ",a)') comment(1:lencom)
            Write(70,'("C Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
        OPEN(71,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'iers_both_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS open of iers site position file is",I5)')IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(71,'("C ",a)') comment(1:lencom)
            Write(71,'("C Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
      end if
      if (do_sinex_non) then
        OPEN(85,IOSTAT=IOS, &
     &    FILE=path(1:ipath)//'iers_solep_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr,'("IOS open of iers site sub (solep) file: ",I5)')IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(85,'("C ",a)') comment(1:lencom)
            Write(85,'("C Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
        OPEN(86,IOSTAT=IOS, &
     &    FILE=path(1:ipath)//'iers_siteid_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr,'("IOS open of iers site sub (siteid) file: ",I5)') &
     &          IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(86,'("C ",a)') comment(1:lencom)
            Write(86,'("C Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
        OPEN(99,IOSTAT=IOS, &
     &    FILE=path(1:ipath)//'iers_solstat_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr, &
     &      '("IOS open of iers site sub (sol stats) file: ",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(99,'("C ",a)') comment(1:lencom)
            Write(99,'("C Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
        OPEN(101,IOSTAT=IOS, &
     &    FILE=path(1:ipath)//'sinex_header_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr, &
     &      '("IOS open of sinex header file: ",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(101,'("C ",a)') comment(1:lencom)
            Write(101, &
     &        '("C Using spool file: ",a, &
     &          "  control file:  ",a,"  covfile: ",a)') &
     &        spool_name(1:lensp), &
     &        control_name(1:lencon),cov_name(1:lencov)
          end if
        end if
        OPEN(102,IOSTAT=IOS, &
     &    FILE=path(1:ipath)//'sinex_inhist_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr, &
     &      '("IOS open of sinex input history file: ",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(102,'("C ",a)') comment(1:lencom)
            Write(102, &
     &      '("C Using spool file: ",a, &
     &        "  control file:  ",a,"  covfile: ",a)') &
     &      spool_name(1:lensp), &
     &      control_name(1:lencon),cov_name(1:lencov)
          end if
        end if
        OPEN(103,IOSTAT=IOS, &
     &    FILE=path(1:ipath)//'sinex_site_recvr_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr, &
     &      '("IOS open of sinex site receiver file: ",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(103,'("C ",a)') comment(1:lencom)
            Write(103,'("C Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
        OPEN(104,IOSTAT=IOS, &
     &    FILE=path(1:ipath)//'sinex_site_ant_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr, &
     &      '("IOS open of sinex site antenna file: ",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(104,'("C ",a)') comment(1:lencom)
            Write(104,'("C Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
        OPEN(105,IOSTAT=IOS, &
     &    FILE=path(1:ipath)//'sinex_site_ecc_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr, &
     &      '("IOS open of sinex site eccentricity file: ",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(105,'("C ",a)') comment(1:lencom)
            Write(105,'("C Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
        OPEN(106,IOSTAT=IOS, &
     &    FILE=path(1:ipath)//'sinex_input_files_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr, &
     &      '("IOS open of sinex input/files file: ",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(106,'("C ",a)') comment(1:lencom)
            Write(106,'("C Using NO solution item files ")')
          end if
        end if
        OPEN(109,IOSTAT=IOS, &
     &    FILE=path(1:ipath)//'file_comment_all_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr, &
     &      '("IOS open of sinex file_comment_all file: ",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(109,'("C ",a)') comment(1:lencom)
            Write(109,'("C Using NO solution item files ")')
          end if
        end if
        OPEN(110,IOSTAT=IOS, &
     &    FILE=path(1:ipath)//'file_ref_all_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr, &
     &      '("IOS open of sinex file_ref_all file: ",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(110,'("C ",a)') comment(1:lencom)
            Write(110,'("C Using NO solution item files ")')
          end if
        end if
        OPEN(111,IOSTAT=IOS, &
     &    FILE=path(1:ipath)//'gps_phase_cent_all_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr, &
     &      '("IOS open of sinex gps_phase_cent_all file: ",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(111,'("C ",a)') comment(1:lencom)
            Write(111,'("C Using NO solution item files ")')
          end if
        end if
        OPEN(112,IOSTAT=IOS, &
     &    FILE=path(1:ipath)//'input_ack_all_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr, &
     &      '("IOS open of sinex input_ack_all file: ",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(112,'("C ",a)') comment(1:lencom)
            Write(112,'("C Using NO solution item files ")')
          end if
        end if
        OPEN(113,IOSTAT=IOS, &
     &    FILE=path(1:ipath)//'sinex_trailer_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr, &
     &      '("IOS open of sinex sinex_trailer file: ",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(113,'("C ",a)') comment(1:lencom)
            Write(113,'("C Using NO solution item files ")')
          end if
        end if
      end if
      if (do_sinex_cov) then
        OPEN(88,IOSTAT=IOS, &
     &    FILE=path(1:ipath)//'iers_solest_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr,'("IOS open of iers site sub (solest) file: ",I5)') &
     &          IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(88,'("C ",a)') comment(1:lencom)
            Write(88, &
     &      '("C Using spool file: ",a, &
     &        "  control file:  ",a,"  covfile: ",a)') &
     &      spool_name(1:lensp), &
     &      control_name(1:lencon),cov_name(1:lencov)
          end if
        end if
        OPEN(89,IOSTAT=IOS, &
     &    FILE=path(1:ipath)//'iers_solmat_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr,'("IOS open of iers site sub (solmat) file: ",I5)') &
     &          IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(89,'("C ",a)') comment(1:lencom)
            Write(89, &
     &        '("C Using covariance file: ",a)') &
     &         cov_name(1:lencov)
          end if
        end if
      end if
!
      if (do_sinex_cap) then
        OPEN(90,IOSTAT=IOS, &
     &    FILE=path(1:ipath)//'iers_solestap_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr,'("IOS open of iers site sub (solestap) file: ",I5)') &
     &          IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(90,'("C ",a)') comment(1:lencom)
            Write(90, &
     &      '("C Using spool file: ",a, &
     &        "  control file:  ",a,"  covfile: ",a)') &
     &      spool_name(1:lensp), &
     &      control_name(1:lencon),cov_name(1:lencov)
          end if
        end if
        OPEN(91,IOSTAT=IOS, &
     &    FILE=path(1:ipath)//'iers_solmatap_'//File_id(1:lid), &
     &    STATUS='UNKNOWN')
        WRITE(qstr,'("IOS open of iers site sub (solmatap) file: ",I5)') &
     &          IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(91,'("C ",a)') comment(1:lencom)
            Write(91, &
     &      '("C Using covariance file: ",a)') &
     &       cov_name(1:lencov)
          end if
        end if
      end if
!
!       Open the min sig files
      if (do_min_sig) then
        OPEN(79,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'gsn_minsig_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS opening of min sig file is",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(79,'("$$ ",a)') comment(1:lencom)
            Write(79,'("$$ Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
      end if
!
!       Open the flyby files
      if (site_flyby) then
        OPEN(50,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'sitmod_pos_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS opening of site mod file is",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(50,'("$$ ",a)') comment(1:lencom)
            Write(50,'("$$ Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
        OPEN(63,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'sitmod_vel_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS opening of site mod file is",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(63,'("$$ ",a)') comment(1:lencom)
            Write(63,'("$$ Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
      end if
      if (source_flyby) then
        OPEN(51,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'srcmod_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS opening of source mod file is",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          if (.not.separate_header_file) then
            Write(51,'("$$ ",a)') comment(1:lencom)
            Write(51,'("$$ Using spoolfile:  ",a)') spool_name(1:lensp)
          end if
        end if
      end if
!
!     Open the hypermap velocity file.
      if (do_cmpar) then
        OPEN(75,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'cmp_pos_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of cmp positions is",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          Write(75,'("MM SPLF 88 1.00 From:",a)') spool_name(1:lensp)
        end if
!
        OPEN(76,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'cmp_cvel_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of cmp velocities is",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          Write(76,'("MM SPLF 88 1.00 From:",a)') spool_name(1:lensp)
        end if
!
        OPEN(77,IOSTAT=IOS, &
     &  FILE=path(1:ipath)//'cmp_tvel_'//File_id(1:lid), &
     &  STATUS='UNKNOWN')
        WRITE(qstr,'("IOS from open of cmp velocities is",I5)') IOS
        call asnl(qstr)
        if (ios .ne. 0) then
          call return_to_continue()
        else
          Write(77,'("MM SPLF 88 1.00 From:",a)') spool_name(1:lensp)
        end if
      end if
!
!
      return
      end
