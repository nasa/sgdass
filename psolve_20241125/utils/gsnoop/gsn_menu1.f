      SUBROUTINE gsn_menu1()
!
!     main menu
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
      INCLUDE 'gsnoop_com.i'
!
      integer*2     ios, ipath, tdisplay, trimlen, len, old_ipath
      integer*2     luerr,lulist
      integer*4     ix1, iy1
      integer*4     ix, iy, ich
      character*4 cch
      equivalence (ich, cch)
!
      LOGICAL*2 kdone, krenew
      LOGICAL*2 FILE_FOUND, k1_1, k1_2
!
      character*6   file_id
      character*10  qspool
      character*39  qsig
      character*36  qheader_dest
      character*78  qblank, comment, path, old_path
      character*79  qstr
      character*8   version_date
      character*1   item_type
      character*63  cf_display
      integer*2     icfd_len
      character*63  old_command_file
      CHARACTER*79  STR_VERS
      CHARACTER     GET_VERSION*54
      INTEGER*2     INT2_ARG
      INTEGER*4     I_LEN,INT4
!
!
      DATA file_found /.false./
      DATA File_id /'junk  '/
      DATA qspool  /'          '/
      DATA qskip_stat /'        '/  !was HRAS 085
      DATA path    /'/tmp/'/
      DATA qsig    /'Use triple sigmas (Using single sigmas)'/
      DATA qheader_dest  /'(H)eader written to top of all files'/
      DATA spool_name    /'none'/
      DATA control_name    /'none'/
      DATA cov_name    /'none'/
      DATA comment /'This file was produced by gsnoop  '/
      data version_date /'00.10.20'/
      data command_file /' '/
      data icflen /0/
!
!     93/07/12  DS Caprette, Hughes STX, added feature permitting selection
!               of spoolfile by specifying solop parameters.
!               Added status=delete to close of spool_list file.
!     94/07/xx  DS Caprette, Hughes STX converted to curses.
!     95/4      K. Baver Produce the 6 1995 iers site submission files.
!                        Look up input covariance file for 4 of these from
!                        the solution catalog.
!                        Install change by JWR to print version.
!     95/07/12 kdb Add ability to use input covariance and control files
!                  that are not catalogued in a solution archiving system.
!     95/10/17 kdb Disable initial skipping of HRAS 085.
!     95/12/12 kdb No longer require user to specify / on end of output file
!                  path.
!     96/6/3   kdb This version changes makefile and .f references from
!                  data17 to data15.
!     96/7/11  kdb Add new option F--command file to filter multiple sets of
!                  solution items through a single interactively set gsnoop
!                  run, into a series of file_ids.
!     97/10/1  kdb Change default comment from "this line deliberately left
!                  blank" to announcement that the file was produced by gsnoop.
!     98/8/4   kdb From now on, open the spool file for processing after the
!                  user is done selecting options.  (That way, if he makes
!                  a mistake, he can reselect the spool file without having
!                  to open and close it.
!     98/8/4   kdb Display the name of the current control and covariance files
!     00/9/12  kdb Disable obsolescent and malfunctioning F (command file)
!                  option.
!                  Also disable leftover fragment of site skipping option
!                    (command 4).
!     00/10/23 kdb Add get_version.
!     01/09/14 kdb Header information (comment and input spool, control and
!                  covariance file paths) may now be put into a separate file
!                  rather than into the top two lines of all output files.
!     03/23/06 kdb Senkr_mn handles its input via an integer*4 variable, which
!                  is subject to linux endian conversion problems.  So 
!                  this subroutine will now transfer senkr_mn's input to a 
!                  character variable and work with that.
!     06/04/18 kdb Add conditional compiler directives to comment out solution
!                  archiving code under LINUX, where it is not available yet.
!
!
!     INCLUDE 'gsnoop_version.i' ! Set revision date of the current version
      luerr = 78
      lulist = 64
      kwp = .false.
      k1_1 = .false.
      k1_2 = .false.
      kdone = .false.
      krenew = .true.
      do_vel = .false.
      do_3_sig = .false.
      do_hyper = .false.
      do_annual = .false.
      kskip_stat = .false.
      site_flyby = .false.
      do_lat_lon = .false.
      do_ep_sites = .false.
      source_flyby = .false.
      bin_uen_sigs = .false.
      do_iers_site = .false.
      do_iers_source = .false.
      scale_by_rrchi = .false.
      do_sinex_non = .false.
      do_sinex_cov = .false.
      do_sinex_cap = .false.
      separate_header_file = .false.
!
      write (qblank, "(78x)")
      ipath = trimlen(path)
!
!
!
!
          ix = 0
          iy = 0
          ix1= 1
          iy1= 21
          call setcr_mn(ix, iy )
          call clear_mn()
          call refresh_mn()
!
 110  do while (.not. kdone)
        if (krenew) then
          ix = 0
          iy = 0
          call setcr_mn(ix, iy )
          call clear_mn()
          call addstr_f("Extract global spool file parameters  " )
          STR_VERS = GET_VERSION()
          CALL SETCR_MN ( 79-I_LEN(STR_VERS), 0 )
          CALL REVERSE_ON_MN()
          CALL ADDSTR_F ( STR_VERS(1:I_LEN(STR_VERS)) )
          CALL REVERSE_OFF_MN()
!
          ix = 0
          iy = 1
          call setcr_mn(ix, iy )
          qstr = "Spool file:      "
!         call reverse_on_mn
          call asnl(qstr )
!         call reverse_off_mn
!
          qstr = "Control file:    "
!         call reverse_on_mn
          call asnl(qstr )
!         call reverse_off_mn
!
          qstr = "Covariance file: "
!         call reverse_on_mn
          call asnl(qstr )
!         call reverse_off_mn
!
          qstr = "(1) Comment: "//comment(1:64)
           call as2nl(qstr )
!
          qstr = "(2) Output path: "//path(1:ipath)
          call as2nl(qstr )
!
          qstr="(3) Output file id: "//file_id//"          "// &
     &               qheader_dest
          call as2nl(qstr )
!
          qstr= &
     &"(l) List spool files                (p) Positions and Velocities"
          call as2nl(qstr )
!
          qstr= &
     &"(c) Enter catalog name              (o) Other output files"
          call as2nl(qstr )
!
          qstr= &
     &"(n) local spoolfile name            (m) Make tables"
          call as2nl(qstr )
!
          qstr= &
     &"(r) rewrite menu screen             (t) Terminate program"
          call as2nl(qstr )
!
          qstr= &
     &"(v) enter input covariance file     "// &
     &"(k) enter input control file "
          call as2nl(qstr )
!
          qstr= &
     &"(a) "//qsig
          call as2nl(qstr )
!
!         if (icflen.gt.0) then
!           cf_display = command_file(1:icflen)
!           icfd_len = icflen
!         else
!           cf_display = 'no file'
!           icfd_len = 7
!         endif
!         qstr=
!    &"command (f)ile: "//cf_display(1:icfd_len)
!         call as2nl(qstr)
!
          call clrtobot_mn()
          call refresh_mn()
          krenew = .false.
        end if
!
        tdisplay = trimlen(spool_name)
        qstr = spool_name(1:tdisplay)//qspool
        len = trimlen(qstr)
        ix = 17
        iy = 1
        call setcr_mn(ix, iy )
        call reverse_on_mn()
        call asnl(qstr )
        call reverse_off_mn()
!
        tdisplay = trimlen(control_name)
        qstr = control_name(1:tdisplay)
        len = trimlen(qstr)
        ix = 17
        iy = 2
        call setcr_mn(ix, iy )
        call reverse_on_mn()
        call asnl(qstr )
        call reverse_off_mn()
!
        tdisplay = trimlen(cov_name)
        qstr = cov_name(1:tdisplay)
        len = trimlen(qstr)
        ix = 17
        iy = 3
        call setcr_mn(ix, iy )
        call reverse_on_mn()
        call asnl(qstr )
        call reverse_off_mn()
!
!       sense response and set flags
!
        call setcr_mn(ix1, iy1 )
        call clrtobot_mn()
        call senkr_mn(ix, iy, ich )
        call casefold (cch)
!
!         if (ich .eq. 49) then !(1)
          if (cch(4:4) .eq. '1') then !(1)
            qstr = "Enter a comment to put into the files."
            call asnl(qstr )
            call getstr_f(comment )
            ix = 13
            iy = 4
            call setcr_mn(ix, iy )
            call addstr_f(comment(1:64) )
            call setcr_mn(ix1, iy1 )
            call clrtobot_mn()
!
!
!         else if (ich .eq. 50) then !(2)
          else if (cch(4:4) .eq. '2') then !(2)
            old_path = path
            old_ipath = ipath
            qstr = "Enter a path for the output files."
            call asnl(qstr )
            call getstr_f(path )
            ipath = trimlen(path)
            if (path(ipath:ipath).ne.'/') then
              if (ipath.lt.78) then
                path(ipath+1:ipath+1) = '/'
                ipath = trimlen(path)
              else
                qstr = "Path must end in / and no room to add one."
                call asnl(qstr )
                qstr = "(Total path exceeds maximum of 78 characters.)"
                call asnl(qstr )
                call return_to_continue()
                path = old_path
                ipath = old_ipath
              endif
            endif
            ix = 17
            iy = 6
            call setcr_mn(ix, iy )
            call clrtoeol_mn()
            call addstr_f(path(1:ipath) )
            call setcr_mn(ix1, iy1 )
            call clrtobot_mn()
!
!
!         else if (ich .eq. 51) then  !(3)
          else if (cch(4:4) .eq. '3') then !(3)
!               clear space in menu
            qstr = "      "
            ix = 20
            iy = 8
            call setcr_mn(ix, iy )
            call addstr_f(qstr(1:6) )
!               get new file id
            call setcr_mn(ix1, iy1 )
            qstr = "Enter a 6-character id for unique file names?"
            call asnl(qstr )
            call getstr_f(file_id )
            call setcr_mn(ix, iy )
!               write it to menu
            call addstr_f(file_id(1:6) )
            call setcr_mn(ix1, iy1 )
            call clrtobot_mn()
!
!
!         else if (ich .eq. 52) then  !(4)
!           qstr ="Do you want to skip a station? (y/(n))"
!           call addstr_f(qstr(1:40))
!           call senkr_mn(ix, iy, ich)
!           kskip_stat = .false.
!           IF(ich .eq. 89) then
!             kskip_stat = .true.
!             qstr = "Station to skip?  "
!             call asnl(qstr)
!             call getstr_f(qskip_stat)
!           ELSE
!             qskip_stat = '        '
!           END IF
!           call setcr_mn(ix1, iy1)
!           call clrtobot_mn
!
!
!         else if (ich .eq. 67) then   !(c)
          else if (cch(4:4) .eq. 'C') then !(c)
            item_type = 'S'
            call get_cat_file(item_type,spool_name, file_found )
            krenew = .true.
!
            if (.not. file_found) then
              qspool = " NOT FOUND"
              qstr = spool_name//" "//qspool
              call asnl(qstr )
              call return_to_continue()
            else
              qspool = "          "
            end if
!
!
!         else if (ich .eq. 78) then  !(n)
          else if (cch(4:4) .eq. 'N') then !(n)
            ITEM_TYPE  = 'S'
            CALL GET_FILE_GEN(ITEM_TYPE,SPOOL_NAME,FILE_FOUND )
            krenew = .true.
            if (file_found) then
              qspool = "         "
            else
              qspool = " NOT FOUND"
            end if
!         else if (ich .eq. 86) then   !(v)
          else if (cch(4:4) .eq. 'V') then !(v)
            item_type = 'V'
            call get_covn_files(item_type,cov_name,file_found )
            krenew = .true.
!         else if (ich .eq. 75) then   !(k)
          else if (cch(4:4) .eq. 'K') then !(k)
            item_type = 'C'
            call get_covn_files(item_type,control_name,file_found )
            krenew = .true.
!         else if (ich .eq. 70) then   !(f)
          else if (cch(4:4) .eq. 'F') then !(f)
!           clear the current command file name from the menu
!           qstr = "                              "//
!    .             "                              "//"   "
!           ix = 16
!           iy = 20
!           call setcr_mn(ix, iy)
!           call addstr_f(qstr(1:63))
!           get new command file name
!           call setcr_mn(ix1, iy1)
!           qstr = "Enter the full path to a command file "//
!    .         "(blank to specify no file)"
!           call asnl(qstr)
!           old_command_file = command_file
!           call getstr_f(command_file)
!           if (index(':',command_file).ne.0) then
!             command_file = old_command_file
!           endif
!           icflen = trimlen(command_file)
!           write it to the menu
!           if (icflen.gt.0) then
!             cf_display = command_file(1:icflen)
!             icfd_len = icflen
!           else
!             cf_display = 'no file'
!             icfd_len = 7
!           endif
!           call setcr_mn(ix, iy)
!           call addstr_f(cf_display(1:icfd_len))
!           call setcr_mn(ix1, iy1)
!           call clrtobot_mn
!         else if (ich .eq. 65) then   !(a)
          else if (cch(4:4) .eq. 'A') then !(a)
            if (do_3_sig .eq. .false.) then
              do_3_sig = .true.
                qsig = "Use single sigmas (Using triple sigmas)"
            else
              do_3_sig = .false.
                qsig = "Use triple sigmas (Using single sigmas)"
            end if
!
            ix = 4
            iy = 20
            call setcr_mn (ix, iy )
            call addstr_f (qsig )
!         else if (ich .eq. 72) then   !(h)
          else if (cch(4:4) .eq. 'H') then !(h)
            if (separate_header_file .eq. .false.) then
              separate_header_file = .true.
              qheader_dest = "(H)eader written to separate file   "
            else
              separate_header_file = .false.
              qheader_dest = "(H)eader written to top of all files"
            end if
!
            ix = 36
            iy = 8
            call setcr_mn (ix, iy )
            call addstr_f (qheader_dest )
!
!
!         else if (ich .eq. 80) then    !(p)
          else if (cch(4:4) .eq. 'P') then !(p)
 210        call gsn_menu1_2 (kdone, k1_1)
            if (k1_1) then
              k1_1 = .false.
              go to 220
            end if
            krenew = .true.
!
!         else if (ich .eq. 79) then   !(o)
          else if (cch(4:4) .eq. 'O') then !(o)
 220        call gsn_menu1_1 (kdone, k1_2)
            if (k1_2) then
              k1_2 = .false.
              go to 210
            end if
            krenew = .true.
!
!         else if (ich .eq. 82) then   !(r)
          else if (cch(4:4) .eq. 'R') then !(r)
            krenew = .true.
            ix = 1
            iy = 1
            call setcr_mn(ix, iy )
!
!
!         else if (ich .eq. 76) then    !(l)
          else if (cch(4:4) .eq. 'L') then !(l)
#ifdef LINUX
!           qstr = "One moment please..."
!           call asnl(qstr )
!           call refresh_mn()
!           call get_spool_list (spool_name, path, file_id, file_found, &
!    &           luerr, lulist )
!           krenew = .true.
!           if (.not. file_found) then
!             qspool = " NOT FOUND"
!             qstr = spool_name//" "//qspool
!             call asnl(qstr )
!             call return_to_continue()
!           else
!             qspool = "          "
!           end if
            qstr = "This SOLARCH option is not available under linux."
            call asnl(qstr )
            call return_to_continue()
#else
!           qstr = "One moment please..."
!           call asnl(qstr )
!           call refresh_mn()
!           call get_spool_list (spool_name, path, file_id, file_found, &
!    &           luerr, lulist )
!           krenew = .true.
!           if (.not. file_found) then
!             qspool = " NOT FOUND"
!             qstr = spool_name//" "//qspool
!             call asnl(qstr )
!             call return_to_continue()
!           else
!             qspool = "          "
!           end if
            qstr = "This SOLARCH option is not available under ftn 90."
            call asnl(qstr )
            call return_to_continue()
#endif
!
!
!         else if (ich .eq. 77) then   !m
          else if (cch(4:4) .eq. 'M') then !(m)
            krenew = .true.
            kdone = .true.
            ix = 0
            iy = 0
            call setcr_mn(ix,iy )
            call clrtobot_mn()
            call refresh_mn()
!
!
!         else if (ich .eq. 84) then     !(t)
          else if (cch(4:4) .eq. 'T') then !(t)
            qstr ="Sure you want to terminate now? (y/n)"
            call asnl (qstr )
            call senkr_mn(ix, iy, ich )
            call casefold(cch)
!           if  (ich .eq. 89) then   !(y)
            if (cch(4:4) .eq. 'Y') then !(y)
              call end_mn()
              stop
            end if
            call setcr_mn(ix1, iy1 )
            call clrtobot_mn()
!
!
          else
            call beep_mn()
!
!
          end if
!
!
      end do  !(.not. kdone)
!
!
!
!     If using a command file, gsnoop will check the spoolfile etc. later.
      if (icflen.ne.0) file_found = .true.
      IF (.NOT. FILE_FOUND) then
        kdone = .false.
        qspool = " NOT FOUND"
        qstr = "No spoolfile found."
        call asnl(qstr )
        call return_to_continue()
        go to 110
!
      else
        if ((do_xyz_vel).or.(do_uen_vel).or. &
     &  (do_uen_adj).or.(do_uen_cpl))    do_vel = .true.
        if (icflen.eq. &
     &    0)call open_files (comment, path, ipath, file_id)
!
      END IF
!
      file_tag = file_id
      user_path = path
      out_comment = comment
!
!
      return
      end
