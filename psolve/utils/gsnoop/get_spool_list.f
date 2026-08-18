      SUBROUTINE get_spool_list (spool_name, path, file_id, file_found, &
     &           luerr, lulist)
!
!     Lets user browse through list of spool files, and open one.
!
!     93/02/24, D.S. Caprette Hughes STX, cloned with extensive modification
!               from gsn_menu1, in gsnoop.
!     93/04/01, D.S. Caprette Hughes STX, added '#', and '/' to menu.
!     93/07/12, D.S. Caprette Hughes STX, added kgot_list and save statement
!               to speed up oeration when subroutine called more than once
!               during the same run.
!     94/06/28  DS Caprette Hughes STX, converted to curses.
!     060323 kdb Senkr_mn handles its input via an integer*4 variable, which
!                is subject to linux endian conversion problems.  So 
!                this subroutine will now transfer senkr_mn's input to a 
!                character variable and work with that.
!     060418 KDB Add conditional compiler directives to comment out solution
!                archiving code under LINUX, where it is not available yet.
!
!
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     Updated to specificaly type integers which
!-------------------------------------------------
!     Called subroutines:  ap_acfile, curses routines
!
!     called functions:
      integer*2 getunit
!
!     Input variables:
      integer*2     luerr    !logical unit for writing error messages
      integer*2     lulist   !logical unit for spool_list
      character*(*) path     !path where spool file list file will be created
      character*(*) file_id  !id tag that will appended to the end of the
!                            !name of the spool file list scratch file name.
!
!     Output variables:
      character*(*)  spool_name
      LOGICAL*2 file_found     !.true. if spool_file found within this routine
!                            !using inquire
!
!     Internal variables:
      integer*2   oldlen, ilen, len, trimlen
      integer*2   i, j, k        !counters, indices
      integer*2   kmax           !number of spool files in spl_name
      integer*2   npat
      integer*2   nstart
      integer*2   ierr           !error registers
      INTEGER*4   IOS
      integer*2   ipath          !number of characters in path
      integer*2   nper           !one less than the number of spoolfiles
!                                !per page of list
!
      integer*4   ix, iy, ich   !curses interface variables
      character*4 cch
      equivalence (ich, cch)
!
!
      LOGICAL*2 kdone
      LOGICAL*2 kgot_list
      logical*4      kexist       !inquire
!
      character*1    qch
      character*3    qch3
      character*8    qtype
      character*60   qpat     !pattern for search
      character*60   qoldpat  !old pattern for searcht
      character*80   qstr     !generic character buffer
      character*79   qhead    !character buffer for header
!
!
      character*79  qmenu(5), qful(6)  !character buffer for menus
!
      data qmenu(1)/ &
     &"(s) to select spool file &
     &OR:"/, &
     &qmenu(2)/ &
     &"(n) Next page            (p) Previous page            (b) Back &
     & tobeginning"/, &
     &qmenu(3)/ "(/) Search on substring  (#) Change display length"/, &
     &qmenu(4)/ &
     &"(f) Full information     (r) Return to previous menu  (t) &
     &Terminate program"/
!
!     spool file info strings:
!
      integer*2      nsp         !ordinal number for specific spool file in list
      integer*2      max_sf
      parameter      (max_sf = 1000)  !room for 1000 spoolfiles in list.
!                                     !If the number of available spool files
!                                     !exceeds 1000 nobody is going to want
!                                     !to browse them anyhow!
!
      character*120  splist_name   !name of file with spool info
      character*230  qbuf(max_sf)  !array of spool info from splist_name
!
!
!
      save j, k, nper, oldlen, qbuf, kmax, kgot_list
!
!
      ios = 0
      nsp = 1
      npat = 0
      oldlen = 1
      qoldpat = "baloney"
      qch = "b"
      kdone = .false.
      ipath = trimlen (path)
      if (luerr .eq.0)  luerr  = GETUNIT()
      if (lulist.eq.0)  lulist = GETUNIT()
!
!
!     Create name for spool file list, create the list, and handle any
!     error returns. Then open the list, and read the records into ASCII
!     arrays.
!
!     The format of the list file is:
!
!     characters 1-2:  user tag
!               3-10:  solution tag (centered format: that is,
!                                       "   2a   "
!                                       "  15    "
!                                       " 701both"
!                                       "1633rep "  )
!              11-12:  version number  (e.g., " 1")
!             13-167:  full path to spool file (if path is smaller than field,
!                      path is left-justified and blanks fill the remaining
!                      space)
!            168-170:  length of full path (e.g., " 41")
!            171-230:  comment - description of the solution version (e.g.,
!                       that run of the solution)
!                         (left-justified in field, with blanks as filler)
!
      if (.not. kgot_list) then  !We only do this once.
!
        nper = 13
        k = 0
!
        len = trimlen(file_id)
        splist_name = "/tmp/spool_list_"//file_id(1:len)
!
        ilen = trimlen(splist_name)
        splist_name = splist_name(1:ilen)
        qstr = "This will take a little while..."
        call asnl (qstr )
        call refresh_mn()
#ifdef LINUX
!cout   call spl_acfile (lulist   , splist_name, luerr, ierr )
#else
!cout   call spl_acfile (lulist   , splist_name, luerr, ierr )
#endif
!
        ix = 0
        iy = 0
        call setcr_mn(ix, iy )
        call clrtobot_mn()
!
!
        if (ierr .eq. 0) then
          qstr = " "
          kgot_list = .true.
        else if (ierr .eq. -1) then
          qstr = "Error occurred opening list file."
        else if (ierr .eq. -2) then
          qstr = "First record of list file may be corrupt."
        else if (ierr .eq. -3) then
          qstr = "Error occurred opening solution catalog."
        else if (ierr .eq. -4) then
          qstr ="Spoolfile information record(s) may be corrupt."
        else if (ierr .eq. -5) then
          qstr ="Error occurred accessing solution catalog."
        else if (ierr .eq. -6) then
          qstr = "First record of list file may be corrupt."
        else if (ierr .eq. -7) then
          qstr = "Idiot!  You''re going to overwrite the catalog"// &
     &          " or a hash file.  It may already be too late."
        else if (ierr .eq. -999) then
          qstr = "I don''t know what you did, but it must have really"// &
     &           " been bad!"
        else
          qstr = "An uncoded error has occurred."
        end if
        call as2nl(qstr )
!
        if (ierr .ne. 0) then
          call return_to_continue()
        end if
!
!       Get # spool files from header.
!
        open (lulist   , file=splist_name, iostat=ios, status='OLD', &
     &      form='UNFORMATTED', access='DIRECT', recl=230)
        read (lulist   , rec=1, iostat=ios) qbuf(1)
!
        if (ios .eq. 0) then
          read (qbuf(1)(53:90), *) kmax
        else
          kgot_list = .false.
          write (qstr, &
     &                     '("Error ",i," reading header in ",a)')ios, splist_name
          call as2nl(qstr )
        end if
!
!       Fill array of spool info.
!
        do k = 1 , kmax
          read (lulist   , rec=(k + 1), iostat=ios) qbuf(k)
          if (ios .ne. 0) then
            kgot_list = .false.
            write (qstr, "('Error ',i,' reading record ',i4, &
     &         ' in ',a)") ios, k, splist_name
            call as2nl(qstr )
          end if
        end do
!
        close(unit=lulist   ,status="DELETE")
!
!       Check for completeness.
!
 110    if (k .le. kmax) then
          qstr = "List may be incomplete."
          call asnl(qstr )
          call return_to_continue()
        end if
!
!       Initialize j and k for list of spool files on screen.
!
        j = 1
        k = j + nper
        if (k .gt. kmax) k = kmax
!
      end if  !(.not.kgot_list)
!
!
!
!
! Display formats:
!
!         Header for spoolfile list
          qhead = "  #  user  glob id  ver                Comment"
!    Spoolfile list
 1011   format ("(",i3,") ",a2,2x,a8,x,a3,2x,a50)
        qful(1) = "user  globe id    ver"
!
!
      call refresh_mn()
!     ich = 78
      cch(4:4) = 'N'
      do while (.not. kdone)
        ix = 0
        iy = 0
        call clrtobot_mn()
!
!120    if ((ich .eq. 78).or.(ich .eq. 80).or.(ich .eq. &
!     &       66).or.(ich .eq. 32).or.(ich .eq. 83)) then
 120    if ((cch(4:4) .eq. 'N').or.(cch(4:4) .eq. 'P').or.(cch(4:4) .eq. &
      &       'B').or.(cch(4:4) .eq. ' ').or.(cch(4:4) .eq. 'S')) then
          ix = 0
          iy = 0
          call setcr_mn (ix, iy )
!
!         Write header for spool listing
          call as2nl(qhead )
!
!         Write list of spool files
          do i =j, j + nper
            if (i .le. kmax) then
              write (qstr, &
     &          1011)i, qbuf(i)(1:2), qbuf(i)(3:10), &
     &           qbuf(i)(11:12),  qbuf(i)(171:220)
            else
              write (qstr, "(79x)")
            end if
            call asnl(qstr )
          end do
!
!         Write menu
          ix = 0
          iy = nper + 3
          call setcr_mn(ix, iy )
          do i = 1,4
            call asnl(qmenu(i) )
          end do
          call clrtobot_mn()
          call refresh_mn()
!
        end if
!
!       Obtain response and do the right thing.
!       If the user entered s then get  index number from menu and
!       set spool_name = full name with path of the spool file.
!       Otherwise, check first character of qch3 to see if it
!       is a menu choice.
!
!
        call senkr_mn (ix, iy, ich )
        call casefold (cch)
!       if (ich .eq. 83) then
        if (cch(4:4) .eq. 'S') then
!
          qstr = "Enter the number in () followed by <RETURN>."
          call asnl(qstr )
          call refresh_mn()
          call getstr_f(qch3 )
          read (qch3, "(i3)", iostat=ios) nsp
!
!         Force nsp to remain within valid limits so stuff doesn't blow up.
!
          if (nsp .gt. kmax) then
           nsp = kmax
          else if (nsp .lt. 0) then
           nsp = 1
          end if
!
          if (ios .ne. 0)  then  !User entered text, not an integer
!           ich = 32
            cch(4:4) = " "
          else if ((nsp .gt. 0).and.(nsp .le. kmax)) then !number is valid
!           if (ich .ne. 70) then                        !open spool file
            if (cch(4:4) .ne. 'F') then                        !open spool file
              read (qbuf(nsp)(168:170), "(i3)") ilen
              spool_name = qbuf(nsp)(13:13 + ilen)
              inquire(FILE=spool_name,EXIST=kexist,iostat=ios)
              if (ios .ne. 0) then
                write (qstr,'("Error ",i4," occurred inquiring about ")') ios
                call asnl(qstr )
                call as2nl(spool_name )
                file_found = .false.
              else if (kexist) then  !We're done here.
                kdone = .true.
                file_found = .true.
              else
                call asnl(spool_name )
                qstr = "NOT FOUND"
                call as2nl(qstr )
                file_found = .false.
                call return_to_continue()
              end if
!              open(40,iostat=ios,file=spool_name,status='old')
!             if (ios .eq. 0) then  !We're done here.
!                kdone = .true.
!               file_found = .true.
!             else
!                write (qstr,'("Error ",i4," occurred opening ",a)')
!     &               ios, spool_name
!                call as2nl(qstr)
!               file_found = .false.
!             end if
            end if
          else
            nsp = 1
          end if
!
!        Full spool file information display,
!        If user has requested full info write info on one spool file only.
!        Then state options: full info on another or return to list.
!
!       else if (ich .eq. 70) then
        else if (cch(4:4) .eq. 'F') then
          do while (nsp .ne. 0)
            qstr = &
     &      "Enter the spoolfile number, or ''0'' to return to list."
            call nl_mn()
            call asnl(qstr )
            call getstr_f(qch3 )
            read (qch3, "(i3)") nsp
            i = nsp
            if (i .gt. kmax) then
              i = kmax
            else if (i .le. 0) then
              i = 1
              continue
            else
              read (qbuf(i)(168:170),*) ilen
!
!             Write full info display
              write (qful(2),'(a2,5x,a8,2x,a3)') &
     &           qbuf(i)(1:2), qbuf(i)(3:10), qbuf(i)(11:12)
              write (qful(3),'("comment: ",a)')   qbuf(i)(171:230)
              write (qful(4),'("full path: ",a)') qbuf(i)(13:13 + ilen)
              do i = 1,4
                call asnl(qful(i) )
              end do
            end if
!
          end do
!         ich = 66
          cch(4:4) = 'B'
!
!
!       Next page of spool files.
!
!       else if (ich .eq. 78) then
        else if (cch(4:4) .eq. 'N') then
!         ich = 78
          cch(4:4) = 'N'
          j = k + 1
          if (j .gt. kmax) then
            j = 1
          end if
          k = j + nper
          if (k .gt. kmax) k = kmax
!
!       Previous page of spool files.
!
!       else if (ich .eq. 80) then
        else if (cch(4:4) .eq. 'P') then
!         ich = 80
          cch(4:4) = 'P'
          j = j - 1 - nper
          if (j .le. 0) j = kmax - nper
!
          k = j + nper
!
!       First page of spool files.
!
!       else if (ich .eq. 66) then
!         ich = 66
        else if (cch(4:4) .eq. 'B') then
          cch(4:4) = 'B'
          j = 1
          k = j + nper
!
!       Change number of spool files displayed
!
!       else if (ich .eq. 35) then
        else if (cch(4:4) .eq. '#') then
          write (qstr,"('Enter the # lines to be displayed per page.')")
          call asnl (qstr )
!         ich = 35
          cch(4:4) = '#'
          call getstr_f(qstr )
          read (qstr, *) nper
          nper = nper - 1
          k = j + nper
          if (k .gt. kmax) k = kmax
!
!       Search
!
!        else if (ich .eq. 47) then
         else if (cch(4:4) .eq. '/') then
           qoldpat = qpat(1:oldlen)  !save in case we're repeating it
!
           qstr = "Enter the pattern for search: "
           len = trimlen(qstr) + 2
           call addstr_f(qstr(1:len) )
           call getstr_f(qpat )
           if (qpat .eq. "") qpat = qoldpat(1:oldlen)
           oldlen = trimlen(qpat)
!
           write (qstr ,"('Search for ',a,' in:  ', &
     &           '(s)olution id or (c)omment? ' $)") qpat(1:oldlen)
           call nl_mn()
           call asnl(qstr )
           call senkr_mn (ix, iy, ich )
           call casefold (cch)
!
!          if (ich .eq. 83) then
           if (cch(4:4)   .eq. 'S') then
             qtype = "solution"
!          else if (ich .eq. 67) then
           else if (cch(4:4)   .eq. 'C') then
             qtype = "comment"
           end if
!
           if (qpat(1:oldlen) .ne. qoldpat(1:oldlen)) then
             nstart = 1          !start at beginning
           else
             nstart = j + 1 !do this for next search
           end if
!
           call search_spool_list (nstart, kmax, npat, qpat, qbuf, &
     &                             qtype, ierr )
           if (ierr .eq. 0) then
             j = npat
             k = j + nper
             if (k .gt. kmax) k = kmax
           else if (ierr .eq. -1) then
             write (qstr , &
     &              "(a,' not found between ',i3,' and ',i3)")qpat(1:oldlen), nstart, kmax
             call as2nl(qstr )
             call return_to_continue()
             j = 1
             k = j + nper
             if (k .gt. kmax) k = kmax
           end if
!          ich = 32
           cch(4:4) = ' '
!
!       Try again
!
!       else if (ich .eq. 48) then
!         ich = 48
        else if (cch(4:4) .eq. '0') then
          cch(4:4) = '0'
!
!       Return to caller
!
!       else if ((ich .eq. 82).or.(ich .eq. 58)) then
        else if ((cch(4:4) .eq. 'R').or.(cch(4:4) .eq. ':')) then
          kdone = .true.
!
!
!       End it all.
!
!       else if (ich .eq. 84) then
        else if (cch(4:4) .eq. 'T') then
            qstr = "Sure you want to terminate now? (y/n)"
            call addstr_f(qstr(1:39) )
            call senkr_mn(ix, iy, ich )
            call casefold (cch)
!           if  (ich .eq. 89) then
            if (cch(4:4) .eq. 'Y') then
              call end_mn()
              stop
            end if
            call nl_mn()
!
!       Huh?
!
        else if (ich .ne. 32) then
          continue
!
!
        end if
!
!
      end do
!
!
!
      return
      end
