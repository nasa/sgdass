      SUBROUTINE site_id_sect(upath,ftag,sta_min_disps,sta_max_disps, &
     &                        qcpl_stat,qmcpl_local,ncplstat, &
     &                        cpls_nepochs,cpls_min_disp,cpls_max_disp, &
     &                        isolns)
!
!     SETS UP THE SITE/ID SECTION OF THE 1995 IERS SITE SUBMISSION
!     ALSO SETS UP THE SINEX SITE/RECEIVER, SITE/ANTENNA AND
!     SITE/ECCENTRICITY SECTIONS.
!
!
!     written 3/24/95 by kdb
!
!     modifications
!
!     kdb 4/10/95 Convert from program to subroutine for use in gsnoop
!     kdb 4/12/95 Merged antenna_short.dat with antenna.dat
!     kdb 7/19/96 Add sinex site/receiver, site/antenna and site/eccentricity
!                  sections.
!     kdb 7/24/96 Add extra digit to the height field in the site/id file.
!                 Also move the height, longitude and latitude fields closer
!                 together, remove the F/M (fixed/mobile) symbol, and
!                 remove the R (VLBI technique code).  These things will
!                 convert from the sinex .04 version to the .05 version.
!     kdb 980227  Move antenna.dat to solve_files for the benefit of Solve.
!                 Also merge new information into antenna.dat for use by Solve.
!                 (This requires some old information to be squeezed down
!                 to accomodate the new information.).
!     kdb 980730  Upgrade to sinex 1.00
!     kdb 980807  Handle output to sinex_site_ant_<>,
!                                  sinex_site_ecc_<>,
!                                  sinex_site_recvr_<>
!                 for continuous piecewise linear sites.
!     kdb 000928  Get the cpl output for the iers_siteid_<> file from
!                 antenna.dat and append it to iers_siteid_<>.  (The user
!                 must insert it manually into the proper place, but at least
!                 now the information will be generated for the user, as a
!                 first step towards automating cpl output.)
!     kdb 000928  Until now, cpl output has been appended to the
!                 sinex_site_ant_<>, sinex_site_ecc_<>, sinex_site_recvr_<>
!                 and iers_siteid_<> files, and the user has had to manually
!                 place the output at its proper position.  Fix this subroutine
!                 to properly position the cpl output.
!     kdb 001025  Parameterize antenna.dat directory.
!
      IMPLICIT NONE
!
      INCLUDE 'solve.i'
!
!     Input variables:
!
!     upath, ftag - define the path to the iers_sit file for this gsnoop
!                   run
!     sta_min_disps, sta_max_disps - minimum and maximum epochs
!     isolns - indicate the solution numbers at a specific monument.
!           (e.g., 1 for pre-quake, 2 for post-quake position)
!
      character*78 upath
      character*6 ftag
      character*12 sta_min_disps(*),sta_max_disps(*)
      integer*2 isolns(*)
      character*12 cpls_min_disp(1,*), cpls_max_disp(1,*)
      character*4 qmcpl_local(*)
      integer*2 ncplstat,cpls_nepochs(*)
      character*8 qcpl_stat(*)
!
!     Output variables: none
!
      integer*2 max_sites
      parameter (max_sites=500)
      integer*2 max_cpl_sites
      parameter (max_cpl_sites=10)
      character*63 input_file,output_file,des_file
      integer*2 trimlen,inlen,inlu,ierr,outlen,deslen,deslu, &
     &          desnum,ict,d_lat_d,d_lat_m,d_lon_d,d_lon_m,ich_len
      logical*2 loop
      character*4 desmon(max_sites),cdp_mon,cz_lon,cz_lat
      character*1 fm_code(max_sites)
      character*120 cbuf
      character*1 fm_print
      character*9 domes_no,des_domes(max_sites)
      character*22 desdata(max_sites)
      character*21 site_descrip,stars
      real*8 pos(3),rlat,rlon,height,pi,d_lat_s,d_lon_s
      character*8 last_name,des_cdp_name(max_sites),cdp_name
      character*2 des_pq(max_sites),pq_flag
      character*132 cbuffer
      character*80 qstr
      integer*2 ilpath,iltag
      integer*2 ist
      integer*2 mct
      INTEGER*4  IOS
      character*21 cpl_descrip(max_cpl_sites)
      character*22 cpl_descrip_tmp
      character*9 cpl_domes(max_cpl_sites)
      integer*2 cpl_lon_d(max_cpl_sites),cpl_lon_m(max_cpl_sites), &
     &          cpl_lat_d(max_cpl_sites),cpl_lat_m(max_cpl_sites)
      character*4 cpl_lon_s(max_cpl_sites),cpl_lat_s(max_cpl_sites)
      real*8 cpl_lon_s_tmp, cpl_lat_s_tmp
      real*8 cpl_height(max_cpl_sites)
      integer*2 lct
      integer*2 icpl_ct
      logical*2 icpl_printed(max_cpl_sites), all_cpl_printed
!
      DATA PI/3.1415926535D0/
      data stars /"*********************"/
!
      do ict = 1,max_cpl_sites
        icpl_printed(ict) = .false.
      enddo
!
!     open the iers_sit_file just made (the file formatted for pre-1995
!     iers site submissions).
!
      ilpath = trimlen(upath)
      iltag = trimlen(ftag)
      input_file = upath(1:ilpath)//'iers_sit_'//ftag(1:iltag)
      inlen = trimlen(input_file)
      inlu = 87
!
      open(inlu,file=input_file(1:inlen),iostat=ios, &
     &     status='old',access='sequential',form='formatted')
      ierr = ios
      if (ierr.ne.0) then
        write(qstr,"('error ',i5,' opening ')") ierr
        call asnl(qstr)
        write(qstr,"(A)") input_file(1:inlen)
        call asnl(qstr)
        write(qstr,"('Cannot create iers_siteid_<usrtag>')")
        call asnl(qstr)
        return
      end if
!
!     Open antenna dat and read it into an array.
!     This provides station descriptions, the domes numbers and the codes for
!     whether the antennas are fixed or mobile
!
      des_file=SOLVE_SAVE_DIR//'antenna.dat'
      deslen = trimlen(des_file)
      deslu = 49
      open(deslu,file=des_file(1:deslen), &
     &     iostat=ios, status='old', &
     &     access='sequential',form='formatted')
      ierr = ios
      if (ierr.ne.0) then
        write(qstr,"('error ',i5,' opening ')") ierr
        call asnl(qstr)
        write(qstr,"(A)") des_file(1:deslen)
        call asnl(qstr)
        write(qstr,"('Cannot create iers_siteid_<usrtag>')")
        call asnl(qstr)
        close(inlu)
        return
      end if
!
      desnum = 0
      loop = .true.
      last_name = '9Z9Z9Z9Z'
      do while (loop)
        read (deslu,"(A132)",end=125) cbuffer
        if (cbuffer(1:1).ne.'#') then
          desnum = desnum + 1
          if (desnum.gt.max_sites) then
            write(qstr,"('too many sites in: ')")
            call asnl(qstr)
            write(qstr,"(A)") des_file(1:deslen)
            call asnl(qstr)
            write(qstr,"('Cannot create iers_siteid_<usrtag>')")
            call asnl(qstr)
            close (inlu)
            close (deslu)
            return
          end if
          read (cbuffer,"(A8,1X,A4,1X,A1,1X,A9,55X,A22,30X)") &
     &      des_cdp_name(desnum),desmon(desnum), &
     &      fm_code(desnum),des_domes(desnum),desdata(desnum)
          if (des_cdp_name(desnum).eq.last_name) then
            des_pq(desnum) = 'pq'
          else
            des_pq(desnum) = '  '
          end if
          last_name = des_cdp_name(desnum)
!         If this is a cpl site, save the description, longitude, latitude and
!         height, because this data is not generated in the iers_sit_<> file
!         for cpl sites and must instead be gotten from antenna.dat.
          do ict = 1,ncplstat
            if (des_cdp_name(desnum).eq.qcpl_stat(ict)) then
              read (cbuffer, &
     &         "(16X,A9,55X,A22,I3,1X,I2,1X,F4.1, &
     &                1X,I3,1X,I2,1X,F4.1,1X,F6.1)") &
     &          cpl_domes(ict),cpl_descrip_tmp, &
     &          cpl_lon_d(ict),cpl_lon_m(ict),cpl_lon_s_tmp, &
     &          cpl_lat_d(ict),cpl_lat_m(ict),cpl_lat_s_tmp, &
     &          cpl_height(ict)
              write (cpl_lon_s(ict),"(f4.1)") cpl_lon_s_tmp
              do lct = 1,4
                if (cpl_lon_s(ict)(lct:lct).eq.' ') &
     &            cpl_lon_s(ict)(lct:lct) = '0'
              end do
              write (cpl_lat_s(ict),"(f4.1)") cpl_lat_s_tmp
              do lct = 1,4
                if (cpl_lat_s(ict)(lct:lct).eq.' ') &
     &            cpl_lat_s(ict)(lct:lct) = '0'
              end do
              ich_len = trimlen(cpl_descrip_tmp)
              if (ich_len.gt.21) then
                cpl_descrip(ict) = stars
              else
                cpl_descrip(ict) = cpl_descrip_tmp
              end if
            endif
          enddo
        end if
      end do
 125  close (deslu)
!
!     Print out the output files.
!
!     First the headers.
!
      write(86,"('+SITE/ID')")
      write(86,"('*CODE PT DOMES____ T ', &
     &              'STATION DESCRIPTION___ ', &
     &              'APPROX_LON_ APPROX_LAT_ APP_H__')")
!
      write(103,"('+SITE/RECEIVER')")
      write(103,"('*Code PT SOLN T Data Start__ Data End____ ', &
     &     'Receiver type_______ S/N__ Firmware_ID')")
!
      write(104,"('+SITE/ANTENNA')")
      write(104,"('*Code PT SOLN T Data Start__ Data End____ ', &
     &     'Receiver type_______ S/N__')")
!
      write(105,"('+SITE/ECCENTRICITY')")
      write(105,"('*                                         ', &
     &     '    UP_____ NORTH__ EAST___')")
      write(105,"('*Code PT SOLN T Data Start__ Data End____ ', &
     &     'typ ARP --> benchmark (m)_______')")
!
!     Now print the data for each site in alphabetical order.  The loop will be
!     driven by the iers_sit_<> file, which contains the non-cpl sites in
!     alphabetical order.   The loop will also check the cpl site name array on
!     each pass to determine when to insert the cpl sites.
!
      ist = 0
      loop = .true.
      do while (loop)
        read(inlu,"(A)",end=500) cbuf
        if (cbuf(1:1).eq.'X') then
          ist = ist + 1
!
!         found another site
!         Decode the site name, post-quake flag, monument number and
!         an x,y and z position (in meters),
!         which can be used to generate
!         the site's latitude and longitude and ellipsoidal height
!
          read(cbuf,"(11X,1X,A8,1X,A2,5X,A4,3(1X,F12.3))") &
     &        cdp_name,pq_flag,cdp_mon,(pos(ict),ict=1,3)
!
!         But first check the cpl site name array to see if it's time to print
!         any cpl sites.  (The loop will print a cpl site immediately before
!         printing the first site that has a lexically greater name.
!         (e.g., to insert HRAS 085 between HOHNBERG and JPL MV1,
!               pass n-1 will print HOHNBERG and
!               pass n   will print HRAS 085, then JPL MV1.)
!         This algorithm assumes that the cpl site name array is sorted
!         alphabetically, so that if two or more cpl sites must be inserted at
!         a spot, they will be guaranteed to be inserted in order.)
!
          do icpl_ct = 1,ncplstat
            if ( (lgt(cdp_name,qcpl_stat(icpl_ct)))    .and. &
     &                (.not. icpl_printed(icpl_ct))   ) then
              icpl_printed(icpl_ct) = .true.
              do mct = 1,cpls_nepochs(icpl_ct)+1
!               site/id line
!               (Only needed once.)
                if (mct.eq.1) then
                  write(86,"(1X,A4,2X,'A',1X,A9,1X,'R',1X,A21,1X, &
     &                        2(1X,I3,1X,I2.2,1X,A4),1X,F7.1)") &
     &             qmcpl_local(icpl_ct),cpl_domes(icpl_ct), &
     &             cpl_descrip(icpl_ct), &
     &             cpl_lon_d(icpl_ct),cpl_lon_m(icpl_ct), &
     &                  cpl_lon_s(icpl_ct), &
     &             cpl_lat_d(icpl_ct),cpl_lat_m(icpl_ct), &
     &                  cpl_lat_s(icpl_ct), &
     &             cpl_height(icpl_ct)
                endif
!               sinex site/receiver section line
                write(103,"(1X,A4,2X,'A',3X,I2,' R ',A12,1X,A12,1X, &
     &                '----VLBI Station----',1X,'--NM-',1X, &
     &                '-----NA----')") &
     &               qmcpl_local(icpl_ct),mct, &
     &               cpls_min_disp(mct,icpl_ct), &
     &               cpls_max_disp(mct,icpl_ct)
!               sinex site/antenna section line
                write(104,"(1X,A4,2X,'A',3X,I2,' R ',A12,1X,A12,1X, &
     &                '-----VLBI Station---',1X,'--NM-')") &
     &               qmcpl_local(icpl_ct),mct, &
     &               cpls_min_disp(mct,icpl_ct), &
     &               cpls_max_disp(mct,icpl_ct)
!               sinex site/eccentricity section line
                write(105,"(1X,A4,2X,'A',3X,I2,' R ',A12,1X,A12,1X, &
     &               'UNE',1X,'   .0000',1X,'   .0000',1X,'   .0000')") &
     &               qmcpl_local(icpl_ct),mct, &
     &               cpls_min_disp(mct,icpl_ct), &
     &               cpls_max_disp(mct,icpl_ct)
              enddo
            endif
          enddo
!
!         Resume/continue printing the current non-cpl site.
!         Generate the geodetic latitude, east longitude and height above
!         the ellipsoidal earth.
!
          call plh(pos,rlat,rlon,height)
!
!         The height is in meters, which is desired.
!         However, the latitude and longitude are in radians and must be
!         converted to degrees, then broken down into degrees, minutes and
!         seconds for the proper display format.
!         Also, plh returns longitude as east longitude (where anything up
!         to 180 degrees West of the Prime Meridian is a negative value)
!         but the IERS site submission requires positive values which
!         increase with eastward movement and range from 0 to 359 degrees)
!
!
!         radians to degrees
!
          rlat = rlat * 180.0D0 / PI
          rlon = rlon * 180.0D0 / PI
!
!         East longitude conversion
!
          if (rlon.lt.0.0D0) rlon = rlon + 360.0D0
!
!         Format for display
!
          call deg_breakdown(rlat,d_lat_d,d_lat_m,d_lat_s)
          call deg_breakdown(rlon,d_lon_d,d_lon_m,d_lon_s)
!
!         Get the site description, domes number and fixed/mobile code
!         from the array read from the antenna information file.
!
          site_descrip = '?????????????????????'
          do ict = 1,desnum
            if (cdp_name.eq.des_cdp_name(ict).and. &
     &          pq_flag.eq.des_pq(ict)) then
              ich_len = trimlen(desdata(ict))
              if (ich_len.gt.21) then
                site_descrip = stars
              else
                site_descrip = desdata(ict)
              end if
              if (fm_code(ict).eq.'f') then
                fm_print = 'F'
              else if (fm_code(ict).eq.'m') then
                fm_print = 'M'
              else
                fm_print = '?'
              end if
              domes_no = des_domes(ict)
            end if
          end do
!
!         Now print out the data for this site.
!
!         zero fill the longitude and latitude seconds fields
!
          write (cz_lon,"(f4.1)") d_lon_s
          do ict = 1,4
            if (cz_lon(ict:ict).eq.' ') cz_lon(ict:ict) = '0'
          end do
          write (cz_lat,"(f4.1)") d_lat_s
          do ict = 1,4
            if (cz_lat(ict:ict).eq.' ') cz_lat(ict:ict) = '0'
          end do
!         site/id line
!         (note: printing fm_print as A1 prints Fixed/Mobile)
          write(86,"(1X,A4,2X,'A',1X,A9,1X,'R',1X,A21,1X, &
     &                  2(1X,I3,1X,I2.2,1X,A4),1X,F7.1)") &
     &       cdp_mon,domes_no,site_descrip, &
     &       d_lon_d,d_lon_m,cz_lon, &
     &       d_lat_d,d_lat_m,cz_lat, &
     &       height
!         sinex site/receiver section line
          write(103,"(1X,A4,2X,'A',4X,I1,' R ',A12,1X,A12,1X, &
     &          '----VLBI Station----',1X,'--NM-',1X,'-----NA----')") &
     &       cdp_mon,isolns(ist),sta_min_disps(ist),sta_max_disps(ist)
!         sinex site/antenna section line
          write(104,"(1X,A4,2X,'A',4X,I1,' R ',A12,1X,A12,1X, &
     &          '-----VLBI Station---',1X,'--NM-')") &
     &       cdp_mon,isolns(ist),sta_min_disps(ist),sta_max_disps(ist)
!         sinex site/eccentricity section line
          write(105,"(1X,A4,2X,'A',4X,I1,' R ',A12,1X,A12,1X, &
     &       'UNE',1X,'   .0000',1X,'   .0000',1X,'   .0000')") &
!    .       A3,1X,F8.4,1X,F8.4,1X,F8.4)")
     &       cdp_mon,isolns(ist),sta_min_disps(ist),sta_max_disps(ist)
        end if
      end do
!
 500  close(inlu)
!
!     Cheat a little here.  There is -- and probably always will be -- only
!     one cpl site, HRAS 085.  And if any are ever added, the preceding logic
!     will print them.  But there is an exception: a cpl site at the end of
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
        call asnl(qstr)
        write(qstr,"( &
     &     'Probably they are at the end of the site list, ')")
        call asnl(qstr)
        write(qstr,"('which Gsnoop is not coded to handle.')")
        call asnl(qstr)
        write(qstr,"('Please tell a Gsnoop programmer.')")
        call asnl(qstr)
        write(qstr,"('(Type any key to acknowlege)')")
        call asnl(qstr)
        call getstr_f(qstr)
      endif
!     Write section endings.
      write(86,"('-SITE/ID')")
      write(103,"('-SITE/RECEIVER')")
      write(104,"('-SITE/ANTENNA')")
      write(105,"('-SITE/ECCENTRICITY')")
!
      end
