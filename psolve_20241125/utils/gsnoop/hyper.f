      Subroutine hyper
!
!     Creates hypermap file of site horizontal velocities and error ellipses.
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'gsnoop_com.i'
!
      INTEGER*2    I, J, msite
      INTEGER*2    Itype(max_sta_gsnoop)
      integer*2    ilatd, ilatm, ilond, ilonm
      integer*4    ix, iy
      Real*8       new_lon
      Real*8       xlatm, xlonm, xlats, xlons
      Real*8       Az_apriori, Vel_apriori
      Logical      ksouth
      character*1  qtype
      character*4  qplate(max_sta_gsnoop)
      character*8  qsite(max_sta_gsnoop)
      character*80 qstr
      character*1  ee_place
      real*8       ellip_print(3,2)
      integer*2    ict,trimlen
      character*1  fm_place,blrep
      character*6  fm_print, fm_type
      character*8  nobl_site
      real*8       rss_sig
      real*8       erb_print(2),vect_mag
      real*8    vect_dir
!
!     Added horizontal velocity adjustments file.
!     Changed name from hyper_file to hyper
!     DS Caprette Hughes STX  93/03/05
!     Added seconds and height column, changed handling of S lat.
!     DS Caprette Hughes STX  93/03/18
!     Added .gmt data files DS Caprette Hughes STX  93/05/12
!     Added new_lon to make .gmt data files psvelomeca compatible
!       DS Caprette Hughes STX  93/05/12
!     Append to various gmt output files
!       (specifically the 360 and regular adj, mod and vel files)
!       the plate on which
!       each site was assumed to live in the input solution.
!       KDB 95/06/14
!     Optionally switch the error ellipse information between the total
!       and adjustment output files.
!       KDB 95/06/15
!     Optionally append site type (fixed or mobile) to gmt_pos file.
!       KDB 95/06/26
!     Optionally replace blanks in output site names with underscores
!       KDB 95/06/26
!     New output file, "rss" file.  KDB 95/06/27
!     New output file, up adjustment file for gmt plotting. KDB 95/09/25.
!     New option, to automatically generate plots of the type requested by
!        Jim Ryan for the 1995 IUGG meeting.  (Requires skipping of some
!        questions.)  K. Baver 12/29/95
!     In "360" files, switch from positive to negative longitude.  KDB 96/8/1
!     New option for replacing blanks in output site names with underscores.
!       Option M only replaces the blanks in the middle of the site name.
!       (Useful for doing NRAO_140 without getting KAUAI___.) KDB 96/8/8
!     Add header lines to sitell_<gsnoop_id> to label the data columns.
!       KDB 97/10/1.
!     Pass the adjustment horizontal sigma velocity (hor_vel_adj_sig) to
!       the gmt_adj_<> and gmt_adj_<>_360 files  for use by iugg1_9507.gmt,
!       the velocity plotting
!       program.  KDB 98/7/15.
!     Change max_sta to max_sta_gsnoop.  KDB 00/10/25.
!     Correct an error found by the Linux Fortran 95 compiler.  Variable
!       vect_dir was mis-declared as an integer, causing this subroutine to
!       abort when it tried to print vect_dir using a real*8 format.
!         KDB 06/4/18.
!     The episodic sites' second occurrences have zeroed east and north
!       aprioris, which cause atan2d to abort under the lyra fortran 95 
!       compiler.  Code a work around for this situation (skipping atan2d and
!       printing zeroes for the entire hyper_ap_<> line.)
!
      if (do_hyper) then
        write (qstr , "('Writing the hypermap files')")
        call as2nl(qstr )
      end if
!     if (do_gmt) then
!       write (qstr , "('Writing the gmt files')")
!     end if
      if (do_3_sig) then
        write (qstr , "('With 3-sigma errors')")
      end if
!
!
      if (do_3_sig) Write(45,"('Errors in this table are 3-sigma')")
!      if (do_3_sig) Write(65,"('Errors in this table are 3-sigma')")
      write (66,"('long lat evel nvel maj min angle name')")
      write (73,"('long lat evel nvel maj min angle name')")
      write (72,"('long lat evel nvel maj min angle name')")
      write (74,"('long lat evel nvel maj min angle name')")
!     Note: horizontal sigma combines the info in maj and min,
!        the sigmas expressed in the semi-major and semi-minor axes,
!        into a single value.
!
      write (82,"( &
     & 'long lat evel nvel maj min angle name plate horiz_sigma')")
      write (84,"( &
     & 'long lat evel nvel maj min angle name plate horiz_sigma')")
      write (67,"('long lat  name')")
      write (94,"('  long       lat       rss  ', &
     &            'xsig   ysig   zsig     name')")
      if (do_lat_lon) then
        write (56,'(1x, &
     &   "             Fix",/, &
     &   "              (A)", &
     &   "                                             Height",/, &
     &   "          CDP vs.", &
     &   "                                             above",/, &
     &   "          mon Mbl            Latitude      ", &
     &   "East longitude    ellipsoid",/, &
     &   "  Site    num (G) Plate  deg min seconds  ", &
     &   "deg  min seconds   (meters)",/, &
     &       1x)')
      endif
!
      if (do_hyper) then
        if (do_auto_gmt_jwr) then
          blrep = 'N'
          fm_place = 'N'
          ee_place = 'B'
        else
          qstr = &
     &      'Replace blanks with underscores in output site names '
          call asnl(qstr )
          qstr = &
     &      '     (Y)/(N)/only in the (M)iddle?'
          call asnl(qstr )
          call getstr_f(qstr )
          read(qstr,"(a1)") blrep
          call casefold(blrep )
          if (trimlen(blrep).eq.0) blrep = 'N'
          if (blrep.ne.'Y'.and.blrep.ne.'N'.and.blrep.ne. &
     &      'M')blrep = 'N'
          qstr = &
     &       'Append antenna type (fixed/mobile) to positions file '// &
     &         '(Y)/(N)?'
          call as2nl(qstr )
          call getstr_f(qstr )
          read(qstr,"(a1)") fm_place
          call casefold(fm_place )
          if (trimlen(fm_place).eq.0) fm_place = 'N'
          if (fm_place.ne.'Y'.and.fm_place.ne.'N') fm_place = 'N'
          qstr = 'Put east/north error ellipses or up error bars'
          call as2nl(qstr )
          qstr = 'in the gmt (T)otals (DEFAULT) '// &
     &           '(A)djustment or (B)oth files?'
          call as2nl(qstr )
          call getstr_f(qstr )
          read(qstr,"(a1)") ee_place
          call casefold(ee_place )
          if (trimlen(ee_place).eq.0) ee_place = 'T'
          if (ee_place.ne.'A'.and.ee_place.ne.'B') ee_place = 'T'
        endif
        do i = 1,3
          do j = 1,2
            ellip_print(i,j) = 0.0D0
          end do
        end do
        erb_print(1) = 0.0D0
        erb_print(2) = 0.0D0
      end if
!
      Do J=1,nsite
        Read(SITE_NAMES(j)(19:23),'(I5)') i
        nobl_site = site_names(j)(1:8)
        if (blrep.eq.'Y') then
          do ict = 1,8
            if (nobl_site(ict:ict) .eq. &
     &        ' ')nobl_site(ict:ict) = '_'
          end do
        else if (blrep.eq.'M') then
          do ict = 1,trimlen(nobl_site)
            if (nobl_site(ict:ict) .eq. &
     &        ' ')nobl_site(ict:ict) = '_'
          enddo
        endif
!
!       Write out the tables
!
        if (do_hyper) then
          Write(45, &
     &    '(2f11.4,6f7.1,1x,a8,i4," Observed")')site_phi(I), site_lon(I), &
     &    Hor_az  (I), Hor_vel (I), &
     &    Hor_error_amp(1,I)*scale, Hor_error_az(1,I), &
     &    Hor_error_amp(2,I)*scale, Hor_error_az(2,I), &
     &    nobl_site,J
!
!         Write out adjusment vector file
          Write(65, &
     &    '(2f11.4,6f7.1,1x,a8,i4," Adjustment")')site_phi(I), site_lon(I), &
     &    Hor_az_adj (I), Hor_vel_adj (I), &
     &    Hor_error_amp(1,I)*scale, Hor_error_az(1,I), &
     &    Hor_error_amp(2,I)*scale, Hor_error_az(2,I), &
     &    nobl_site,J
!
!         Write out the apriori's with 0 error balls.
          Vel_apriori = &
     &          DSQRT(uen_apriori(2,I)**2 + uen_apriori(3,I)**2)
          if (uen_apriori(2,i).eq.0.0D0 .and. uen_apriori(3,i).eq.0.0D0) then
!           The second occurrence of an episodic site gives zeroes.
!           Just zero out the fields.
            az_apriori = 0.0D0
            vel_apriori = 0.0D0
          else
            Az_apriori  = Atan2d(uen_apriori(2,I),uen_apriori(3,I))
            If(Az_apriori .lt. 0.d0) Az_apriori = Az_apriori+360.d0
          endif
          Write(46, &
     &    '(2f11.4,6f7.1,1x,a8,i4," A priori")')site_phi(I), site_lon(I), &
     &    Az_apriori , Vel_apriori, &
     &    0.d0              , 0.d0             , &
     &    0.d0              , 0.d0             , &
     &    nobl_site,J
!
!        end if
!
!       if (do_gmt) then
!
!         Get monument type from antenna.dat via qant_type
!
          qsite(1) = site_names(j)(1:8)
          call qant_type( qsite, INT2((1)), itype )
          if ((itype(1) .eq. 1).or.(itype(1) .eq. 3)) then
            fm_type = "fixed "
          else if ((itype(1) .eq. 2).or.(itype(1) .eq. 4)) then
            fm_type = "mobile"
          else
            fm_type = "??????"
          end if
!         if (site_lon(I) .lt. 0) then
!           new_lon = site_lon(I) + 360    !psvelomeca compatible?????????
!         else
!           new_lon = site_lon(I)
!         end if
          if (site_lon(I) .gt. 0) then
            new_lon = site_lon(I) - 360    !psvelomeca compatible?????????
          else
            new_lon = site_lon(I)
          end if
!
!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
          if ((uen_dot(2,I).ne. 0).or.(uen_dot(3,I).ne.0)) then
            if (ee_place.eq.'T'.or.ee_place.eq.'B') then
              ellip_print(1,1) = Hor_error_amp(1,I)*scale
              ellip_print(2,1) = Hor_error_amp(2,I)*scale
              ellip_print(3,1) = (90 - Hor_error_az(1,I))
            end if
            Write(66, &
     &      '(7(f9.3,1x),2x,a8,1x,a4)')site_lon(I)    , site_phi(I), &
     &      uen_dot(2,I), uen_dot(3,I), &
     &      (ellip_print(ict,1),ict=1,3), &
     &      nobl_site,sol_plate(I)
!
            Write(73, &
     &      '(7(f9.3,1x),2x,a8,1x,a4)')new_lon    , site_phi(I), &
     &      uen_dot(2,I), uen_dot(3,I), &
     &      (ellip_print(ict,1),ict=1,3), &
     &      nobl_site,sol_plate(I)
!
!           Delta_foo is theoretical - observed, we want to convert
!           to observed - theoretical in this table.  I.e. reverse
!           the vectors.
!
            Write(72, &
     &      '(4(f9.3,1x),2x,"0 0 0",2x,a8)')site_lon(i), site_phi(I), &
     &      -delta_east_vel(I), -delta_north_vel(I), &
     &      nobl_site
!
            Write(74, &
     &      '(4(f9.3,1x),2x,"0 0 0",2x,a8)')new_lon    , site_phi(I), &
     &      -delta_east_vel(I), -delta_north_vel(I), &
     &      nobl_site
!
            if (ee_place.eq.'A'.or.ee_place.eq.'B') then
              ellip_print(1,2) = Hor_error_amp(1,I)*scale
              ellip_print(2,2) = Hor_error_amp(2,I)*scale
              ellip_print(3,2) = (90 - Hor_error_az(1,I))
              erb_print(2) = uen_dot_sig(1,i)
            end if
!
            Write(82, &
     &      '(4(f9.3,1x),2x,3(F9.3,1X),1x,a8,1x,a4,1x,f9.3)')site_lon(i), site_phi(I), &
     &      delta_east_vel(I), delta_north_vel(I), &
     &      (ellip_print(ict,2),ict=1,3), &
     &      nobl_site,sol_plate(I),hor_vel_adj_sig(I)
!
            if (delta_up_vel(i) .gt. 0.0D0) then
              vect_dir = 90.0D0
            else
              vect_dir = 270.0D0
            endif
            vect_mag = dabs(delta_up_vel(i))
            Write(96, &
     &      '(4(f9.3,1x),2x,F9.3,1X,1x,a8,1x,a4)')site_lon(i), site_phi(I), &
     &      vect_dir, vect_mag, &
     &      erb_print(2), &
     &      nobl_site,sol_plate(I)
!
            Write(84,'(4(f9.3,1x),2x,3(F9.3,1X),1x,a8,1x,a4, &
     &             1x,f9.3)') &
     &      new_lon    , site_phi(I), &
     &      delta_east_vel(I), delta_north_vel(I), &
     &      (ellip_print(ict,2),ict=1,3), &
     &      nobl_site,sol_plate(I),hor_vel_adj_sig(I)
          end if
!
!         Write out positions file
          if (fm_place .eq. 'Y') then
            fm_print = fm_type
          else
            fm_print = '      '
          endif
          Write(67, &
     &    '(2(f9.3,1x),2x,a8,1X,A6)')site_lon(I), site_phi(I), &
     &    nobl_site,fm_print
!
!         Write rss file
          rss_sig = xyz_sig(1,i) * xyz_sig(1,i) + &
     &              xyz_sig(2,i) * xyz_sig(2,i) + &
     &              xyz_sig(3,i) * xyz_sig(3,i)
          rss_sig = dsqrt(rss_sig)
          write(94, &
     &    '(2(f9.3,1x),2x,4(f6.1,1x),a8)')site_lon(I), site_phi(I), rss_sig, &
     &    xyz_sig(1,i),xyz_sig(2,i),xyz_sig(3,i), &
     &    nobl_site
!
!         Write out model velocity file
          if ((uen_dot(2,I).ne. 0).or.(uen_dot(3,I).ne.0)) then
            Write(69, &
     &      '(4(f9.3,1x),2x,"0.0 0.0 0.0  ",a8,1x,a4)')site_lon(I), &
     &      site_phi(I),(uen_dot(2,I)-delta_east_vel(I)), &
     &      (uen_dot(3,I)-delta_north_vel(I)), &
     &      nobl_site,sol_plate(I)
            Write(80, &
     &      '(4(f9.3,1x),2x,"0.0 0.0 0.0  ",a8,1x,a4)')new_lon, site_phi(I), &
     &      (uen_dot(2,I)-delta_east_vel(I)), &
     &      (uen_dot(3,I)-delta_north_vel(I)), &
     &      nobl_site,sol_plate(I)
          end if
!
!         Write out up file
          Write(68, &
     &    '(4(f9.3,1x),2x,a8)')site_lon(I), site_phi(I), &
     &    uen_dot(1,I), uen_dot_sig(1,I), &
     &    nobl_site
!
        end if
!
!
        if (do_lat_lon) then
!
!         Get plate from sitpl via qsitpl
          qsite(1) = site_names(j)(1:8)
          call qsitpl( qsite, INT2((1)), qplate )
!
!         Get monument type from antenna.dat via qant_type
          call qant_type( qsite, INT2((1)), itype )
          if ((itype(1) .eq. 1).or.(itype(1) .eq. 3)) then
            qtype = "A"
          else if ((itype(1) .eq. 2).or.(itype(1) .eq. 4)) then
            qtype = "G"
          else
            qtype = "?"
          end if
!
!         Check if latitude < 0, i.e. Southern Hemisphere.
!
          if (site_phi(i) .lt. 0.0d0) then
            ksouth = .true.
            site_phi(i) =  dabs(site_phi(i))
          else
            ksouth = .false.
          end if
!
          ilatd = iidint(site_phi(i))
          xlatm = (site_phi(i) - ilatd)*6.0d1
          ilatm = iidint(xlatm)
          xlats = (xlatm - ilatm)*6.0d1
          if (ksouth) ilatd = 0 - ilatd ! Convert back if necessary
!
!         convert to 0 < site_lon < 360 if needed
!
          if (site_lon(i) .lt. 0) site_lon(i) = 3.6d2 + site_lon(i)
          ilond = iidint(site_lon(i))
          xlonm = (site_lon(i) - ilond)*6.0d1
          ilonm = iidint(xlonm)
          xlons = (xlonm - ilonm)*6.0d1
          write (56,"(a8,2x,a4,x,a,2x,a4,2x,i3,2x,i2.2,2x,f6.3, &
     &                         3x,i3,2x,i2.2,2x,f6.3,3x,f8.3)") &
     &      nobl_site, site_names(j)(9:12), qtype, qplate(1), &
     &      ilatd, ilatm, xlats, ilond, ilonm, xlons, height(i)
        end if
!
      End do
      End
