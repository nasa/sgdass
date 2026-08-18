      program gsnoop
!
!     PROGRAM TO snoop  the global parameters in a spoolfile
!       (Note: Also now snoops a some arc parameters.)
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     modifications
!
!     Ability to print the 6 output files for the 1995 iers site submission.
!     K. Baver 95/04/07
!
!     New "other page" option (to print gmt files of the type made for
!        Jim Ryan's 1995 iugg presentation).  Also includes fix: closing
!        some files that are not closed by the end of the program.  Makes a
!        difference if a gsnoop feature needs to use some of the files
!        internally at the end, as the new feature needs to use the gmt_mod
!        file.  (If read the file before it's closed, may only have part
!        written, e.g., 892 bytes). K. Baver 12/29/95
!
!     New file, iers_solstat_<>.  (Close it.) K. Baver 4/24/96
!
!     No longer hard code reference epoch in iers_solest_<>
!     and iers_solestap_<> files to 88:001. K. Baver 4/25/96
!
!     No longer close iers_siteid_<> or iers_solep_<> files.  This is done
!     elsewhere in output_annual.f to allow sorting of these files.
!     K. Baver 4/26/96
!
!     New option F--command file to filter multiple sets of solution items
!     through a single interactively set gsnoop run, into a series of
!     file_ids.
!     K. Baver 7/11/96
!
!     Add new files, sinex header line and sinex input history file.
!     K. Baver 7/18/96
!
!     Add new files for sinex receiver, sinex antenna and sinex eccentricity
!     files.
!     K. Baver 7/19/96
!
!     Add new file for sinex input/files section.  K. Baver 7/22/96
!
!     Open spool file after the gsn_menu1 call, instead of within it (during
!     menu selection.)   This way the user can change his mind about which
!     spool file he wants to use during menu selection without an error.
!             K. Baver 8/4/98
!
!     Y2K fixes.  K. Baver 11/20/98
!
!     Disable unused option (others page, A).   K. Baver 9/1/00.
!
!     Change max_sta to max_sta_gsnoop.  K. Baver 10/25/00.
!
!     Close new lus (109-113) for files for sinex sections applying to all
!        sinex runs.  K. Baver 11/1/00.
!
!     Call new subroutine, get_control_info, to start getting some general
!        information from the control file.  Initially only information from
!        two batch control file keywords will be returned and put in
!        gsnoop_com.i.  Other information will be added later as needed.
!            K. Baver 9/20/01.
!
!     Now call new subroutine, spool_top, to get any necessary information
!        from the top of the spool file and put it in common for later use
!        and to perform any other spoolfile initialization (such as
!        determining any spoolfile formats.
!
!            K. Baver 5/15/02.
!
!     Two calls to ftqsort fixed to correct for wrong type in first argument.
!     Jim Ryan 03.05.01
!
!     Add conditional compiler directives to comment out solution
!       archiving code under LINUX, where it is not available yet.
!         K. Baver 4/18/06.
!
      integer*2     i, ncoldat
      integer*4     ix, iy
      character*79  qstr
      integer*2 ierr
      integer*2 icomline
      logical*2 process_loop
      character*128 cbuffer
      integer*2 iitem
      character*2 file_spec_type
      character*63 current_file
      character*2 user_tag
      character*8 sol_tag
      character*5 ver_tag
      integer*2 iver
      integer*2 ilocu
      character*1 ccheck,cformat
      character*1 item
      character*157 fpath
      integer*2 flen,plen,kerr
      character*140   ppath
      integer*2 iarsc
      character*255   message
      logical*4 kexist
      integer*2 ipath,trimlen
      logical*2 items_error
      character*3 item_type
      integer*2 itime(5),iyear,idoy
      integer*4 isecs
      integer*4 ios
      character*2 direction
      data item_type/'SCV'/
!
      INCLUDE 'gsnoop_com.i'
      integer*2 isite_sorts(25,max_sta_gsnoop)
      character*25 site_sorts(max_sta_gsnoop)
      equivalence (site_sorts,isite_sorts)
!
!     Use p kdebug = .true. for debugging
!
      INCLUDE 'gsnoop_version.i' ! Set revision date of the current version
      CALL SET_SIGNAL_CTRLC ( 1 )
      kdebug = .false.
      do_auto_gmt_jwr = .false.
      call start_mn()
      ix = 1
      iy = 1
      call setcr_mn(ix, iy )
      do i = 1, 100
        call nl_mn()
      end do
      call setcr_mn(ix, iy )
      call clrtobot_mn()
      call refresh_mn()
!
! Get instructions via menus.
      call gsn_menu1()
!
!     open the spool file
!
      open(40,iostat=ios,file=spool_name,status='old')
      if (ios .ne. 0) then
        write (qstr, &
     &    '("Error ",i4," occurred opening ",a)')ios, spool_name
        call asnl(qstr )
        stop
      end if
!
!     open the command file, if any
!
      if (icflen.ne.0) then
        open(100,file=command_file(1:icflen),iostat=ios, &
     &       status='old',form='formatted',access='sequential')
        ierr = ios
        if (ierr.ne.0) then
          write(qstr,"('Error ',i5,' opening command file')") ierr
          call asnl(qstr )
          call return_to_continue()
          go to 900
        else
          icomline = 0
          ilocu = 7
          ccheck = ' '
          cformat = 'N'
        endif
      endif
!
!
      process_loop = .true.
      do while (process_loop)
!       Get the creation time of the output files.
        call timeget(itime,iyear )
        if (iyear.lt.2000) then
          iyear = iyear - 1900
        else
          iyear = iyear - 2000
        endif
        idoy = itime(5)
        isecs = itime(4)*3600 + itime(3)*60 + itime(2)
        write (file_create_time, &
     &            "(i2.2,':',i3.3,':',i5.5)")iyear,idoy,isecs
!
!       If the user has specified a file of commands, get the next line,
!       which gives input files and an output file id for the current
!       processing loop.
!
        if (icflen.ne.0) then
          read(100,"(A)",iostat=ios,end=900) cbuffer
          ierr = ios
          if (ierr.ne.0) then
            write(qstr,"('Error ',i5,' reading command file')") ierr
            call asnl(qstr )
            write(qstr,"('The error is at line ',i5)") icomline + 1
            call asnl(qstr )
            call return_to_continue()
            go to 900
          endif
          icomline = icomline + 1
          call splitstring(cbuffer,file_tag,cbuffer )
          ipath = trimlen(user_path)
          do iitem = 1,3
!           Get the next input item's location from the command file
            call splitstring(cbuffer,file_spec_type,cbuffer )
            items_error = .false.
            if (trimlen(file_spec_type).ne.1) then
              items_error = .true.
            else if (file_spec_type.eq.'n') then
              current_file = ' '
            else if (file_spec_type.eq.'f') then
              call splitstring(cbuffer,current_file,cbuffer )
            else if (file_spec_type.eq.'t') then
              call splitstring(cbuffer,user_tag,cbuffer )
              call splitstring(cbuffer,sol_tag,cbuffer )
              call splitstring(cbuffer,ver_tag,cbuffer )
              read(ver_tag,*) iver
              item = item_type(iitem:iitem)
#ifdef LINUX
!cout         call &
!cout&             item_path_t(ilocu,ccheck,cformat,user_tag,sol_tag,iver, &
!cout&             item,fpath,flen,ppath,plen,iarsc,message,kerr )
#else
!cout         call &
!cout&             item_path_t(ilocu,ccheck,cformat,user_tag,sol_tag,iver, &
!cout&             item,fpath,flen,ppath,plen,iarsc,message,kerr )
#endif
              if (kerr .ne. 0) then
                write(qstr,"('Error looking up item ',a2,1X,a8,1X, &
     &            i5,' in the catalog')") user_tag,sol_tag,iver
                call asnl(qstr )
                write(qstr,"('Error message from item_path_t: ')")
                call asnl(qstr )
                write(6,"(A)") message
                qstr = ' '
                call asnl(qstr )
                write(qstr,"('The error is at line ',i5)") icomline
                call asnl(qstr )
                call return_to_continue()
                go to 900
              else
                current_file = fpath(1:flen)
              end if
            else
              items_error = .true.
            endif
            if (items_error) then
              write(qstr,"('Format error in your control file')")
              call asnl(qstr )
              write(qstr,"('The error is at line ',i5)") icomline
              call asnl(qstr )
              write(qstr,"('Specify spoolfile control_file ', &
     &          'covariance_file in that order')")
              call asnl(qstr )
              write(qstr,"('For each item, specify:')")
              call asnl(qstr )
              write(qstr,"('  for actual file name - f file_name')")
              call asnl(qstr )
              write(qstr,"('  for catalog lookup - ', &
     &          't user_tag sol_tag version_number')")
              call asnl(qstr )
              write(qstr,"('  for no such item - n')")
              call asnl(qstr )
              call return_to_continue()
              go to 900
            endif
!           Verify the item's existence.  (For the spoolfile, this will be
!           done by actually opening the spoolfile.)
            if (file_spec_type.ne.'n') then
              if (iitem.eq.1) then ! spoolfile
                open(40,file=current_file,iostat=ios, &
     &             status='old',access='sequential',form='formatted')
                ierr = ios
                if (ierr.ne.0) then
                  write(qstr,"('Error ',i5,' opening spool file ', &
     &              'number ',i5)") ierr,icomline
                  call asnl(qstr )
                  call return_to_continue()
                  goto 900
                else
                  spool_name = current_file
                endif
              else
                inquire(FILE=current_file,exist=kexist,iostat=ios)
                ierr = ios
                if (ierr .ne. 0) then
                  write (qstr,'("Error ",i5," inquiring about ")') ierr
                  call asnl(qstr )
                  qstr = current_file
                  call asnl(qstr )
                  write(qstr,"('The error occurred in command file ', &
     &              'line ',i5)") icomline
                  call asnl(qstr )
                  call return_to_continue()
                  goto 900
                else if (.not.kexist) then
                  qstr = current_file
                  call asnl(qstr )
                  qstr = 'does not exist'
                  call asnl(qstr )
                  write(qstr,"('The error occurred in command file ', &
     &              'line ',i5)") icomline
                  call asnl(qstr )
                  call return_to_continue()
                  go to 900
                else
                  if (iitem.eq.2) then
                    control_name =  current_file
                  else
                    cov_name =  current_file
                  endif
                endif
              endif !verifying one of the three types
            endif !a file of the current type was selected
          enddo !running over all three item types
          call open_files(out_comment,user_path,ipath,file_tag )
        endif !command file was selected
!
!   Get any information from the top of the spool file and perform
!   initialization (such as determining certain spoolfile area formats).
!
        call spool_top()
!
! Input from global results portion of spoolfile
!
        if (do_vel .or. do_hyper  .or. bin_uen_sigs .or. &
     &             do_lat_lon.or. do_annual .or. do_iers_site .or. &
     &             site_flyby.or. do_cmpar  .or. &
     &             do_min_sig.or. &
     &             do_sinex_non.or. do_sinex_cov .or. &
     &    do_sinex_cap)call gsn_main
        if (do_iers_source .or. source_flyby) call get_glob_source
!
! Sort the site names
! The epochs must be expanded to 4 digits to properly sort past 2000.
!
        direction = '24'
        call site_sort_prep(direction,nsite,site_names,site_sorts )
        Call ftqsort( site_sorts, nsite, INT2(25) )
        direction = '42'
        call site_sort_prep(direction,nsite,site_names,site_sorts )
!
        direction = '24'
        call site_sort_prep(direction,nmin,min_site,site_sorts )
        Call ftqsort( site_sorts, nmin, INT2(25) )
        direction = '42'
        call site_sort_prep(direction,nmin,min_site,site_sorts )
!
! Input from experiments (arc) section of spoolfile
        if (do_iers_site .or. do_iers_source .or. &
     &        source_flyby.or. &
     &        do_sinex_non)call get_arc_info
!
!     & .or. site_flyby) call get_arc_info
!
! Scaling of sigmas
        if (scale_by_rrchi) then
          scale = rrchi
        else
          scale = 1.0d0
        end if
        if (do_3_sig) scale = scale*3.0d0
!
! Get selected information from the batch control file and put in gsnoop_com.i.
!
        if (control_name(1:4).ne.'none') then
          call get_control_info()
        end if
!
! Output
        if (bin_uen_sigs) call topo_bin
        if (do_vel .or. site_flyby .or. do_cmpar) call global_info
        if (do_hyper .or. do_lat_lon) call Hyper
        if (do_annual .or. &
     &      do_iers_site.or. do_sinex_non .or. &
     &      do_sinex_cov.or. do_sinex_cap) call Output_Annual
        if (do_iers_source .or. source_flyby) call output_source
        if (do_uen_cpl) call write_cpl
        if (do_min_sig) call min_sig_out
        qstr = "DONE with this pass"
        call as2nl(qstr )
!
! Cleanup
        CLOSE(40)
        CLOSE(41)
        CLOSE(42)
        CLOSE(43)
        CLOSE(44)
        CLOSE(45)
        CLOSE(46)
        CLOSE(47)
        CLOSE(48)
        CLOSE(49)
        CLOSE(50)
        CLOSE(51)
        CLOSE(52)
        CLOSE(53)
        CLOSE(54)
        CLOSE(55)
        CLOSE(56)
        CLOSE(57)
        CLOSE(58)
        CLOSE(59)
        CLOSE(60)
        CLOSE(61)
        CLOSE(62)
        CLOSE(63)
        CLOSE(64)
        CLOSE(65)
        CLOSE(66)
        CLOSE(67)
        CLOSE(68)
        CLOSE(69)
        CLOSE(70)
        CLOSE(72)
        CLOSE(73)
        CLOSE(74)
        CLOSE(75)
        CLOSE(76)
        CLOSE(77)
        CLOSE(79)
        CLOSE(80)
        CLOSE(81)
        CLOSE(82)
        CLOSE(84)
!     Lus 85 and 86 are closed elsewhere now.
        CLOSE(88)
        CLOSE(89)
        CLOSE(90)
        CLOSE(94)
        CLOSE(95)
        CLOSE(96)
        CLOSE(99)
        CLOSE(101)
        CLOSE(102)
        CLOSE(103)
        CLOSE(104)
        CLOSE(105)
        CLOSE(106)
        if (do_sinex_non) then
          CLOSE(109)
          CLOSE(110)
          CLOSE(111)
          CLOSE(112)
          CLOSE(113)
        endif
        if (do_iers_source) close(115)
!
        ncoldat = 10
        if (do_uen_cpl) call plot_uen (spool_name, cpluen_name, ncoldat, &
     &             qcpl_stat, ncplstat)
!       if (do_auto_gmt_jwr) call auto_gmt
        if (icflen.eq.0) process_loop = .false.
      enddo
 900  continue
      call end_mn()
!
!
      end
