      SUBROUTINE gsn_main()
!
!     PROGRAM TO snoop  the site parameters from a spoolfile.
!        Now handles either arc or global parameters.
!
!     modifications:
!
!     95/06/14 kdb Put into gsnoop common
!                  the names of the plates on which the sites
!                  were assumed to live in the input solution.
!     95/07/14 kdb Fix bug in which annual xyz position table only lists
!                  first epoch given a long format spool file station table.
!     96/04/25 kdb Extract reference epoch from the global site section of
!                  the spoolfile
!                  to elminate hard coding of the reference epoch in the
!                  iers site submission output files.
!     96/07/10 kdb Handle site arc parameters.
!     96/08/23 kdb Initialize site_names array to blanks.  Necessary for batch
!                  mode.  Otherwise, leftover names from previous passes may
!                  cause problems, such as a floating exception in the
!                  epoch_form95 call. (A leftover Wettzell appeared in the
!                  first slot ("n+1") past the true names for a pass and
!                  appeared to be an episodic site, encouraging get_arc_info
!                  to place the Wettzell julian date in slot n+1, not slot n.
!                  Slot n was left initialized to a large julian date, causing
!                  the floating point exception.)
!     00/10/25 kdb Change max_sta to max_sta_gsnoop.
!     02/05/15 kdb The parameter index in solve spoolfile adjustment lines is
!                  being enlarged from a four character field to five
!                  characters.  Update to read the new format.
!     03/01/09 kdb Change from separate changeover variables to an array.
!                  Add julian date changeover array to common.
!     03/01/09 kdb Update to read two changes in the spoolfile format made in
!                  the 2002.12.26 solve release.  The changes expand the
!                  formats of the station position epoch and the station
!                  correlation reference epochs from yy/mm/dd and yymmdd,
!                  respectively, to yyyy.mm.dd-hh:mm:ss.
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
      INCLUDE 'gsnoop_com.i'
!
      integer*2 mmin
      integer*2 iyear, neps
      INTEGER*4 IOS
      integer*2 I, ii,jj,kk,ll,m,n
      integer*2 iyr, imn, idy
!
      integer*4 ix, iy
      integer*4 nrec, oldrec
!
      real*8  year
      Real*8  degrees_per_radian, twopi, site_pos(3)
!
      character*5   qform
      character*8   qsub
      character*80  qstr
      character*100 BLANK
!
      LOGICAL*2 kdone, kepoch, keps, kcpls
      character*1 ga_type
      integer*2 ict
!
      integer*2 icol_adj
      real*8 fjldy
!
      data twopi /+6.2831853071795865/
      degrees_per_radian = 360.d0/twopi
      neps = 0
      qsub = "gsn_main"
!
!
      ix = 0
      iy = 0
      nmin = 0
      if (.not. kdebug) call setcr_mn (ix, iy)
      call clrtobot_mn()
      call refresh_mn()
      write (qstr , "('gsnoop main subroutine')")
      call as2nl(qstr )
!
      write (blank,'(100x)')
      nsite = 0
      do ict = 1,max_sta_gsnoop
        site_names(ict) = ' '
      enddo
      nrec = 0
      oldrec = 0
!
      icol_adj = 0
      if (spool_solve_revision_date_jd .ge. &
     &    changeover_jd(1))icol_adj = 1
!
!     Assume the solution was global, until proven otherwise.
      ga_type = 'g'
!
      Kdone = .false.
      Do While (.not.kdone)
        kcpls = .false.
        CBUF   = BLANK
        READ(40,'(A)',END=910) CBUF
        NREC = NREC+1
!       Look for the presence of a "1Run" record before the site parameter
!       records.  This will indicate an arc solution.
        if (cbuf(1:4).eq.'1Run') ga_type = 'a'
        if (kdebug.and.((nrec - oldrec) .gt. 1000)) then
          if (.not. kdebug) then
            ix = 1
            iy = 2
            call setcr_mn (ix, iy )
          end if
          write (qstr , "(i6,' records read')") nrec
          call asnl(qstr )
          oldrec = nrec
        end if
!
!       Look for the begining of station global data
!       Do episodic motion records, if any, differently.
!       When we find "RT. ASC." then we're past the station global
!       data and into the global source positions.
!
        if ((CBUF(16:23) .eq. &
     &  "RT. ASC.").or.(CBUF(17+icol_adj:24+icol_adj) .eq. "RT. ASC.")) &
     &    kdone = .true.
!
!       Look for the spoolfile line that gives the reference epoch.
!       (This is right before the site parameters.)
!
        if(cbuf(1:32).eq.'Station positions are for epoch:') then
          if (spool_solve_revision_date_jd .ge. changeover_jd(2)) then
            read(cbuf(36:37),"(i2)") ref_ep_yr
            read(cbuf(39:40),"(i2)") ref_ep_mn
            read(cbuf(42:43),"(i2)") ref_ep_dy
          else
            read(cbuf(34:35),"(i2)") ref_ep_yr
            read(cbuf(37:38),"(i2)") ref_ep_mn
            read(cbuf(40:41),"(i2)") ref_ep_dy
          end if
        endif
!
        IF ((CBUF(26+icol_adj:28+icol_adj) .eq. ' X ').and. &
     &    (cbuf(1:5+icol_adj) .ne. blank(1:5+ &
     &  icol_adj)))then !Site found
          nsite = nsite + 1
          IF (CBUF(29+icol_adj:32+icol_adj) .ne. 'Comp') then
!          episodic or cpl motion
!
!           skip station only applies to episodic sites
            keps = .true.
            if ((kskip_stat).and. &
     &         (cbuf(7+icol_adj:14+icol_adj).eq.qskip_stat)) then
              nsite = nsite - 1  !set nsite back as we're not using this one.
              go to 1999
            end if
            if (do_uen_cpl) then
              do i = 1, ncplstat
                if (cbuf(7+icol_adj:14+icol_adj) .eq. &
     &            qcpl_stat(i))kcpls = .true.
              end do
            end if
            neps = neps + 1
            if (.not. kdebug) then
              ix = 1
              iy = 5
              call setcr_mn(ix, iy )
            end if
            write (qstr , "('# sites = ',i3,' - ',i3, &
     &                   ' (episodic sites)')") nsite, neps
            call asnl(qstr )
!
            Read(CBUF(29+icol_adj:34+icol_adj),'(3i2)', &
     &          iostat=ios)iyr, imn, idy
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
          else
            keps = .false.
            iyr = 0
            imn = 0
            idy = 0
          end if
          write (SITE_NAMES(nsite)(13:18), "(3i2.2)") iyr, imn, idy
!
          if (.not. kdebug) then
            iy = 5
            ix = 1
            call setcr_mn(ix, iy )
          end if
          write (qstr , "('# sites = ',i3)") nsite
          call asnl(qstr )
!
!
          Read(CBUF(7+icol_adj:14+icol_adj),'(A8)', &
     &      iostat=ios)SITE_NAMES(nsite)(1:8) !CDP name
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
          Write(SITE_NAMES(nsite)(19:23),'(i5)') nsite        !index
          Read(CBUF(16+icol_adj:19+icol_adj),'(A4)', &
     &      iostat=ios)SITE_NAMES(nsite)(9:12)!monument #
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!
!         Get x information
          Read(CBUF(38+icol_adj:52+icol_adj),'(f15.2)', &
     &      iostat=ios)xyz(1,nsite)
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
          Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &       iostat=ios)xyz_sig(1,nsite)
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!         Get y information
          Read(40,'(A)',END=910) CBUF
!
!         if we find a continuous piecewise linear station with the format
!         in which all of the X's are grouped together then extract the
!         information and put it in its own file:
!
!
          if ((cbuf(26+icol_adj:28+icol_adj) .eq. &
     &        ' X ').or.(kcpls)) then
            call cplstat (iyr, imn, idy, kcpls )
            nsite = nsite - 1  !overwrite with next entry
            go to 1999
          end if
!
          NREC = NREC+1
          Read(CBUF(38+icol_adj:52+icol_adj),'(f15.2)', &
     &      iostat=ios)xyz(2,nsite)
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
          Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &      iostat=ios)xyz_sig(2,nsite)
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!         Get z information
          Read(40,'(A)',END=910) CBUF
          NREC = NREC+1
          Read(CBUF(38+icol_adj:52+icol_adj),'(f15.2)', &
     &      iostat=ios)xyz(3,nsite)
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
          Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &      iostat=ios)xyz_sig(3,nsite)
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!         Get the uen adjustments
!
!         Get u information
          Read(40,'(A)',END=910) CBUF
          NREC = NREC+1
          Read(CBUF(38+icol_adj:52+icol_adj),'(f15.2)', &
     &      iostat=ios)uen(1,nsite)
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
          Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &      iostat=ios)uen_sig(1,nsite)
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!         Get e information
          Read(40,'(A)',END=910) CBUF
          NREC = NREC+1
          Read(CBUF(38+icol_adj:52+icol_adj),'(f15.2)', &
     &      iostat=ios)uen(2,nsite)
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
          Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &      iostat=ios)uen_sig(2,nsite)
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!         Get n information
          Read(40,'(A)',END=910) CBUF
          NREC = NREC+1
          Read(CBUF(38+icol_adj:52+icol_adj),'(f15.2)', &
     &      iostat=ios)uen(3,nsite)
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
          Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &      iostat=ios)uen_sig(3,nsite)
!
!         Compute the station longitude and geodetic latitude in degrees.
!         First convert the site position to meters for PLH.
          Do I=1,3
            Site_pos(I) = xyz(I,nsite)/1000.d0
          Enddo
          Call &
     &      PLH(Site_pos, site_phi(nsite), site_lon(nsite), height(nsite))
          site_phi(nsite) = site_phi(nsite)*degrees_per_radian
!
!         convert to degrees
          site_lon(nsite) = site_lon(nsite)*degrees_per_radian
!         Note:  We leave height in meters
!
!         We've now gotten the desired site information (position parameters)
!         for an arc solution.  Now it's just a question of reading past any
!         extraneous records (such as the site's clock and atmosphere
!         records) to get to the next site (if any are left).
!
          if (ga_type.eq.'a') then
            do while (cbuf(27+icol_adj:32+icol_adj).ne.'X Comp'.and. &
     &        CBUF(2:29).NE.'Corrected Reduced Chi-Square')
               read(40,'(A)',END=910) cbuf
               NREC = NREC + 1
            enddo
            if (CBUF(2:29).EQ.'Corrected Reduced Chi-Square') then
!             No more sites
              kdone = .true.
!             Reinitialize in case this is a multiple pass gsnoop run
              epoch_count = 0
            else
!             We found the next site.  But put the site record back
!             temporarily, because the code will try to read the first
!             record later.
              backspace(40)  !backspace before next read into cbuf
              NREC = NREC - 1
            endif
            goto 1975
          endif
!
!         Get the velocities.
!         Skip if we're already at the correlations or adjustments.
!         Or go to the annual positions section, if we are already there.
!
          Read(40,'(A)',END=910) cbuf
          NREC = NREC+1
          if ((CBUF(16:23) .eq. &
     &      "RT. ASC.").or.(CBUF(17+icol_adj:24+icol_adj) .eq. "RT. ASC.")) &
     &       kdone = .true.
          if ((cbuf(2:13).ne.'Correlations').and..not.kdone.and. &
     &      (cbuf(27:53).ne.'Adjustment Position Vector:') )then
            Read(CBUF(40+icol_adj:52+icol_adj),'(f15.2)', &
     &        iostat=ios)xyz_dot(1,nsite)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
            Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &        iostat=ios)xyz_dot_sig(1,nsite)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!           Get the name of the plate on which the site was assumed to live
!           in the input solution.
            READ(CBUF(21+icol_adj:24+icol_adj),'(A4)', &
     &        iostat=ios)sol_plate(nsite)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!           Get y information
            Read(40,'(A)',END=910) CBUF
            NREC = NREC+1
            Read(CBUF(40+icol_adj:52+icol_adj),'(f15.2)', &
     &        iostat=ios)xyz_dot(2,nsite)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
            Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &        iostat=ios)xyz_dot_sig(2,nsite)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!           Get z information
            Read(40,'(A)',END=910) CBUF
            NREC = NREC+1
            Read(CBUF(40+icol_adj:52+icol_adj),'(f15.2)', &
     &        iostat=ios)xyz_dot(3,nsite)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
            Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &        iostat=ios)xyz_dot_sig(3,nsite)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!           Get the neu velocities, velocity adjustments, and sigmas
!           Do up first
            Read(40,'(a)',END=910) cbuf
            NREC = NREC+1
            Read(CBUF(40+icol_adj:52+icol_adj),'(f15.2)', &
     &        iostat=ios)uen_dot    (1,nsite)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
            Read(CBUF(83+icol_adj:92+icol_adj),'(f15.2)', &
     &        iostat=ios)uen_dot_sig(1,nsite)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
            Read(CBUF(61+icol_adj:70+icol_adj),'(f10.3)', &
     &        iostat=ios)Delta_up_vel(nsite)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!           If the up sigma is 0, for the up rate to be 0.
            If(uen_dot_sig(1,nsite) .lt. .01) uen_dot(1,nsite) = 0.d0
!
!           Now do east
            Read(40,'(A)',END=910) CBUF
            NREC = NREC+1
            Read(CBUF(40+icol_adj:52+icol_adj),'(f15.2)', &
     &        iostat=ios)uen_dot     (2,nsite)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
            Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &        iostat=ios)uen_dot_sig (2,nsite)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
            Read(CBUF(61+icol_adj:70+icol_adj),'(f10.3)', &
     &        iostat=ios)Delta_east_vel(nsite)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!           At last do north
            Read(40,'(A)',END=910) CBUF
            NREC = NREC+1
!
            Read(CBUF(40+icol_adj:52+icol_adj),'(f15.2)', &
     &      iostat=ios)uen_dot      (3,nsite)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
            Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &      iostat=ios)uen_dot_sig  (3,nsite)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
            Read(CBUF(61+icol_adj:70+icol_adj),'(f10.3)', &
     &      iostat=ios)Delta_north_vel(nsite)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!           Calculate uen a priori vectors
!
            uen_apriori(1,nsite) = 1.d9
            uen_apriori(2,nsite) = &
     &        uen_dot(2,nsite) - Delta_east_vel (nsite)
            uen_apriori(3,nsite) = &
     &        uen_dot(3,nsite) - Delta_north_vel(nsite)
!
!           Get the horizontal velocity & error ellipse information
!           Initialize horizontals and error ellipse info.
!
            do jj = 1,3
              Error_az (jj,nsite) = 0.0d0
              Error_el (jj,nsite) = 0.0d0
              Error_amp(jj,nsite) = 0.0d0
              pos_error_az (jj,nsite) = 0.0d0
              pos_error_el (jj,nsite) = 0.0d0
              pos_error_amp(jj,nsite) = 0.0d0
            end do
!
            do jj = 1,2
              Hor_error_az (jj,nsite) = 0.d0
              Hor_error_amp(jj,nsite) = 0.d0
              pos_hor_error_az (jj,nsite) = 0.0d0
              pos_hor_error_amp(jj,nsite) = 0.0d0
            end do
!
            Tot_az (nsite) = 0.0d0
            Tot_el(nsite) = 0.0d0
            Tot_vel(nsite) = 0.0d0
            Tot_az_adj (nsite) = 0.0d0
            Tot_el_adj(nsite) = 0.0d0
            Tot_vel_adj(nsite) = 0.0d0
!
            Hor_az (nsite) = 0.0d0
            Hor_az_sig (nsite) = 0.0d0
            Hor_az_adj (nsite) = 0.d0
            Hor_vel(nsite) = 0.0d0
            Hor_vel_adj(nsite) = 0.d0
            Hor_vel_sig(nsite) = 0.0d0
!
            pos_tot_az (nsite) = 0.0d0
            pos_tot_el (nsite) = 0.0d0
            pos_tot_adj(nsite) = 0.0d0
            pos_hor_az (nsite) = 0.0d0
            pos_hor_adj(nsite) = 0.0d0
            pos_hor_az_sig (nsite) = 0.0d0
            pos_hor_adj_sig(nsite) = 0.0d0
!
!           Read next record.  It should be Total velocity, Correlations,
!           annual positions, or global source positions.
            Read(40,'(a)',END=910) cbuf
            NREC = NREC+1
            ii = 1
            jj = 1
            kk = 1
          ll = 1
!
!
            do while ((cbuf(2:13).ne. &
     &          'Correlations').and.(cbuf(7:10).ne. &
     &          'Year').and.(CBUF(16:23).ne. &
     &          "RT. ASC.").and.(CBUF(17+icol_adj:24+icol_adj).ne."RT. ASC."))
!
!             Do velocity error ellipse.
              if (cbuf(32:53).eq. &
     &        'Total Velocity Vector:')then
                Read(cbuf(70:75),*,iostat=ios)    Tot_az (nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
                Read(cbuf(89:93),*,iostat=ios)   Tot_el(nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
                Read(cbuf(106:115),*,iostat=ios) Tot_vel(nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
              else if (cbuf(32:55).eq. &
     &        'Total Horizontal Vector:')then
                Read(cbuf(70:75),*,iostat=ios)    Hor_az (nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
                Read(cbuf(106:115),*,iostat=ios) Hor_vel(nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
              else if (cbuf(32:55).eq. &
     &        'Total Horizontal Sigma: ')then
                Read(cbuf(70:75),*, &
     &          iostat=ios)Hor_az_sig (nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
                Read(cbuf(106:115),*, &
     &          iostat=ios)Hor_vel_sig(nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
              else if (cbuf(27:53).eq. &
     &                    'Adjustment Velocity Vector:')then
!
                Read(cbuf(70:75),*, &
     &          iostat=ios)Tot_az_adj (nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
                Read(cbuf(89:93),*, &
     &          iostat=ios)Tot_el_adj(nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!
                Read(cbuf(106:115),*, &
     &          iostat=ios)Tot_vel_adj(nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
              else if (cbuf(27:55).eq. &
     &                    'Adjustment Horizontal Vector:')then
!
                Read(cbuf(70:75),*, &
     &          iostat=ios)Hor_az_adj (nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
                Read(cbuf(106:115),*, &
     &          iostat=ios)Hor_vel_adj(nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
              else if (cbuf(27:55).eq. &
     &                    'Adjustment Horizontal Sigma: ')then
!
              Read(cbuf(70:75),*, &
     &        iostat=ios)Hor_az_adj_sig(nsite)
              If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
              Read(cbuf(106:115),*, &
     &        iostat=ios)Hor_vel_adj_sig(nsite)
              If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
              else if (cbuf(38:58).eq. &
     &        'Error Ellipsoid Axis:')then
!
                Read(cbuf(70:75),*, &
     &          iostat=ios)Error_az (jj,nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
                Read(cbuf(89:93),*, &
     &          iostat=ios)Error_el (jj,nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
                Read(cbuf(106:115),*, &
     &          iostat=ios)Error_amp(jj,nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
                jj = jj + 1
              else if (cbuf(29:58).eq. &
     &        'Horizontal Error Ellipse Axis:')then
                Read(cbuf(70:75),*, &
     &          iostat=ios)Hor_error_az (kk,nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
                Read(cbuf(106:115),*, &
     &          iostat=ios)Hor_error_amp(kk,nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
                kk = kk + 1
!
!             Do position error ellipse.
              else if (cbuf(27:53).eq. &
     &        'Adjustment Position Vector:')then
                Read(cbuf(70:75),*, &
     &          iostat=ios)pos_tot_az (nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
                Read(cbuf(89:93),*, &
     &          iostat=ios)pos_tot_el (nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
                Read(cbuf(106:115),*, &
     &          iostat=ios)pos_tot_adj(nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
              else &
     &          if(cbuf(13:55).eq. &
     &          'Position Adjustment Horizontal Vector:') &
     &        then
!
                Read(cbuf(70:75),*, &
     &          iostat=ios)pos_hor_az (nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
                Read(cbuf(106:115),*, &
     &          iostat=ios)pos_hor_adj(nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
              else &
     &          if(cbuf(13:54).eq. &
     &        'Position Adjustment Horizontal Sigma:')then
!
                Read(cbuf(70:75),*, &
     &          iostat=ios)pos_hor_az_sig (nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
                Read(cbuf(106:115),*, &
     &          iostat=ios)pos_hor_adj_sig(nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
              else if (cbuf(29:58).eq. &
     &        'Position Error Ellipsoid Axis:')then
                Read(cbuf(70:75),*, &
     &          iostat=ios)pos_error_az (ii,nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
                Read(cbuf(89:93),*, &
     &          iostat=ios)pos_error_el (ii,nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
                Read(cbuf(106:115),*, &
     &          iostat=ios)pos_error_amp(ii,nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
                ii = ii + 1
              else &
     &        if(cbuf(20:58).eq. &
     &        'Position Horizontal Error Ellipse Axis:')then
                Read(cbuf(70:75),*, &
     &          iostat=ios)pos_hor_error_az (ll,nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
                Read(cbuf(106:115),*, &
     &          iostat=ios)pos_hor_error_amp(ll,nsite)
                If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
                ll = ll + 1
              end if
!
              Read(40,'(a)',END=910) cbuf
              NREC = NREC+1
            end do
          if (CBUF(16:23) .eq. "RT. ASC." .or. &
     &        CBUF(17+icol_adj:24+icol_adj) .eq. "RT. ASC.") &
     &        kdone = .true.
          end if
!
!         Get the correlations
          if (cbuf(2:13).eq.'Correlations') then
!
!           Get the reference epoch of the global coordinates and skip a line
            if (spool_solve_revision_date_jd .ge. changeover_jd(2)) then
              Read(CBUF(58:65),'(i2,1x,i2,1x,i2)', &
     &          iostat=ios)(irefdate(m,nsite),m=1,3)
            else
              Read(CBUF(32:37),'(3i2)', &
     &          iostat=ios)(irefdate(m,nsite),m=1,3)
            endif
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
            Read(40,'(a)',END=910) cbuf
            NREC = NREC+1
!
!           Now get the correlations
            n = nsite
            Read(40,1000) y_x (n) !instead of cbuf
            Read(40,1000) z_x (n),z_y (n) !instead of cbuf
            Read(40,1000) xd_x(n),xd_y(n),xd_z(n) !instead of cbuf
            Read(40,1000) yd_x(n),yd_y(n),yd_z(n),yd_xd(n) !instead of cbuf
            Read(40,1000) zd_x(n),zd_y(n),zd_z(n),zd_xd(n),zd_yd(n)!no cbuf read
 1000       Format(10x,5(f8.3))
!           Now skip one line.
            READ(40,'(A)',END=910) CBUF
            NREC = NREC+7
          end if
!
!         Read next record and check for minimum sigma, annual positions,
!         source table or whatever.
!
          Read(40,'(a)',END=910) cbuf
!
!         Get information on stations with minimum sigma turned on
!
          NREC = NREC+1
          if (cbuf(2:14).eq.'Minimum sigma') then
            nmin = nmin + 1
            Read(CBUF(17:),'(i3,2x,a8,x,a4, 16x,a6)', &
     &          iostat=ios) &
     &          mmin, min_site(nmin)(1:8),min_site(nmin)(9:12), &
     &          min_site(nmin)(13:18)
!
            Write(MIN_SITE(nmin)(19:23),'(i5)') nmin        !index
!
!           get min x
            READ(40,'(A)',END=910) CBUF
            Read(CBUF(38+icol_adj:52+icol_adj),'(f15.2)', &
     &        iostat=ios)xyz_min(1,nmin)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
            Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &        iostat=ios)xyz_min_sig(1,nmin)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!           get min y
            READ(40,'(A)',END=910) CBUF
            Read(CBUF(38+icol_adj:52+icol_adj),'(f15.2)', &
     &        iostat=ios)xyz_min(2,nmin)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
            Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &        iostat=ios)xyz_min_sig(2,nmin)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!           get min x
            READ(40,'(A)',END=910) CBUF
            Read(CBUF(38+icol_adj:52+icol_adj),'(f15.2)', &
     &        iostat=ios)xyz_min(3,nmin)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
            Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &        iostat=ios)xyz_min_sig(3,nmin)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!           get min u
            READ(40,'(A)',END=910) CBUF
            Read(CBUF(38+icol_adj:52+icol_adj),'(f15.2)', &
     &        iostat=ios)uen_min(1,nmin)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
            Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &        iostat=ios)uen_min_sig(1,nmin)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!           get min e
            READ(40,'(A)',END=910) CBUF
            Read(CBUF(38+icol_adj:52+icol_adj),'(f15.2)', &
     &        iostat=ios)uen_min(2,nmin)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
            Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &        iostat=ios)uen_min_sig(2,nmin)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!           get min n
            READ(40,'(A)',END=910) CBUF
            Read(CBUF(38+icol_adj:52+icol_adj),'(f15.2)', &
     &        iostat=ios)uen_min(3,nmin)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
            Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &        iostat=ios)uen_min_sig(3,nmin)
            If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
          end if
!
!         Get the annual positions xyz values and sigmas.
          kepoch = .true.
          epoch_count = 0
          Do While (kepoch.and..not.kdone)
            if (CBUF(16:23) .eq. "RT. ASC." .or. &
     &          CBUF(17+icol_adj:24+icol_adj).eq. &
     &          "RT. ASC.")kdone = .true.
            If(cbuf(2:5) .eq. 'Year') then !Epoch values found in long form
              epoch_count = epoch_count+1
              qform = 'long'
              Read(cbuf(7:),*,iostat=ios) year
              iyear = dint (year)
              If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
              If(nsite.ne.1  .and. iyear.ne.epochs(epoch_count)) then
                qstr = " Error in epoch counts"
                call asnl(qstr )
              Else
                epochs(epoch_count) =  iyear
              Endif
              m = epoch_count
              n = nsite
              Read(40,'(a)',END=910) cbuf
              NREC = NREC+1
!
 1001         FORMAT (36X,f16.2,29x,f12.3)
              Read(cbuf,1001, &
     &        iostat=ios)xyz_epoch(1,m,n),xyz_epoch_sig(1,m,n)
              If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
              Read(40,'(a)',END=910,iostat=ios) cbuf
              NREC = NREC+1
!
              Read(cbuf,1001, &
     &        iostat=ios)xyz_epoch(2,m,n),xyz_epoch_sig(2,m,n)
              If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
              Read(40,'(a)',END=910) cbuf
              NREC = NREC+1
!
              Read(cbuf,1001, &
     &        iostat=ios)xyz_epoch(3,m,n),xyz_epoch_sig(3,m,n)
              If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
              Read(40,'(a)',END=910) cbuf
              NREC = NREC+1
!
            Else If(cbuf(7:10) .eq. 'Year') then ! found in short form
              epoch_count = epoch_count+1
              qform = 'short'
              Read(cbuf(12:13),'(i2)',iostat=ios) iyear
              If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
              If(nsite.ne.1  .and. iyear.ne.epochs(epoch_count)) then
                qstr = "Error in epoch counts"
                call asnl(qstr )
              Else
                epochs(epoch_count) =  iyear
              Endif
              m = epoch_count
              n = nsite
!
              Read(cbuf(37:), *, iostat=ios) xyz_epoch(1,m,n)
              If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
              Read(cbuf(54:), *, iostat=ios) xyz_epoch_sig(1,m,n)
              If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
              Read(cbuf(66:), *, iostat=ios) xyz_epoch(2,m,n)
              If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
              Read(cbuf(84:), *, iostat=ios) xyz_epoch_sig(2,m,n)
              If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
              Read(cbuf(95:), *, iostat=ios) xyz_epoch(3,m,n)
              If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
              Read(cbuf(102:), *, iostat=ios) xyz_epoch_sig(3,m,n)
              If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
            Else
              kepoch = .false.
            Endif
          Enddo
 1975   continue
        Endif !Site found
!
!
 1999   if ((kdebug).and.(cbuf(7+icol_adj:14+ &
     &      icol_adj).eq.qskip_stat)) then
          write (qstr ,'("skipping ",a)') qskip_stat
          call asnl(qstr )
        end if
      Enddo
!
!
 910  return
      end
