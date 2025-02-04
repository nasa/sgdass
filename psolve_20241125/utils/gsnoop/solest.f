      SUBROUTINE solest(cvrf_path,ilen,control_path,icontrol_len, &
     &                  ap_choice,ref_ep_yr,ref_ep_mn,ref_ep_dy, &
     &                  covs_wanted,cons_override,sta_all_disp, &
     &                  ncplstat,qcpl_stat)
!
      implicit none
!
!     restrictions -
!
!      Handles site parameters fully.  However,
!      only partially handles eop parameters.
!      (Requires a user input constraint level, and cannot handle anything
!      but an x,y and ut1 offset.)
!      Does not handle any other parameter type at all.
!
      INCLUDE 'solve.i'
!
!     written by mwh 3/95 as a standalone program
!
!     purpose: produces the solution/estimate section of the 1995 style
!              iers site submission (as either apriori or non-apriori files)
!
!     input arguments:
!
!     cvrf_path,ilen - path to input solve covariance file
!     control_path,icontrol_len - path to input solve control file and length
!     ap_choice - determines whether the apriori or non-apriori version
!                 of the file should be generated.
!                 (note that this option just affects the wording of the
!                  begining and ending
!                  delimiter lines and the name of the file to which
!                  the information is
!                  written.  The type of data produced is determined by a
!                  SOLVE option which writes one of the two types of
!                  information to the covariance file which is read by
!                  this subroutine.)
!     ref_ep_yr, ref_ep_mn, ref_ep_dy - reference epoch
!     covs_wanted - bit array of desired types of covariances
!     cons_override - 0-2 to override the setting of the constraint level
!                     to the input value.
!     sta_all_disp - if set to an epoch, then every site in the solution
!            made its first and last appearance at this epoch
!            (indicating a one-arc solution), and the epoch
!            should be used for the epochs in the solest and solestap files.
!            (The epoch comes from the wobble line of the spoolfile.)
!            Otherwise, this variable should be set to _individual_
!            (indicating a global solution in which different sites appeared
!            in different epoch ranges), and the
!            reference epoch from the spoolfile's station table should be used.
!     ncplstat - number of continuous piecewise linear (cpl) sites
!                (as designated by the user on the positions menu)
!     qcpl_stat - list of user-designated cpl sites
!
!
      character*63 cvrf_path
      character*63 control_path
      integer*2 ilen,icontrol_len
      logical*2 ap_choice
      integer*2 ref_ep_yr,ref_ep_mn,ref_ep_dy
      integer*2 covs_wanted, cons_override
      character*12 sta_all_disp
      integer*2 ncplstat
      character*8 qcpl_stat(*)
!
!     output arguments: none
!
!
!     modifications
!
!     kdb 4/10/95 incorporated into gsnoop.  Print out header.
!                 Print out apriori version. Print variable constraint levels.
!                 Look up input covariance file from solution catalog.
!     kdb 4/19/95 More sophisticated generation of constraint levels.  Read
!                 in sites which are exceptions to the general constraint
!                 levels and should be set directly to a specific value.
!                 Added implicit none.
!     kdb 4/25/96 Remove hard coding of reference epoch to 88:001.
!     kdb 7/1/96  Originally handled CVRFxx files from a solution with
!                 the $OUTPUT section COVARIANCES keyword set to CGM STA.
!                 Now handles more general CVRFxx file formats. (One problem
!                 was a varying number of header lines.  Also previously only
!                 handled site parameters.  Now handles eop parameters (but
!                 offsets only, and constraint levels cannot be automatically
!                 set.  Essentially the current change only guarantees to
!                 support site and eop parameters from a solution with
!                 COVARIANCES set to BY_ARC ALL).)
!     kdb 7/24/96 Increase the length of the output sigma fields from E9.4 to
!                 E13.8.
!     kdb 7/26/96 For a one-arc solution,
!                 get the epochs for the solest and solestap files from
!                 the spool file's wobble line, not its station table.
!     kdb 7/30/96 Fix bug: Change the label in the solestap file from
!                 ESTIMATED VALUE and STD_DEV to APRIORI VALUE and APR_STD_DEV.
!    kdb 11/12/97 move solve.i from /data15 to /data18
!     kdb 2/28/97 Move antenna.dat to solve_files directory for the benefit
!                 of solve.
!    kdb 7/30/98  Upgrade to sinex 1.00.
!    kdb 11/20/98 Y2K fixes.
!    kdb 4/22/99  relative include reference, now that gsnoop has joined the
!                 standard source directory tree.
!    kdb 9/27/00  Fix error:  Write proper epoch numbers in the soln fields
!                 of the iers_solest_<> and iers_solestap_<> files for cpl
!                 sites.  (The number 2 used to be written, forcing the user to
!                 manually correct the files.)
!    kdb 10/11/00 Fix error: don't abort if site not found in antenna.dat.
!    kdb 10/25/00 Parameterize antenna.dat directory.
!    kdb  9/19/01 Two new arguments are being returned by the cons_levs
!                 subroutine (velocity tie status and $CONSTRAINTS VELOCITIES
!                 status).  These will be used elsewhere in gsnoop for id'ing
!                 affected sites in the cvel_<> and tvel_<> files.  But dummy
!                 arguments must also be added here.
!    kdb  4/18/06 Add conditional compiler directives to comment out solution
!                 archiving code under LINUX, where it is not available yet.
!
      character*8 sta,compsta
      character*4 mon,pt2
      character*11 ptype
      real*8 sig,adj,tot
      character*128 buf,buffer
      integer*2 i,j,k,first,last,npar
      integer*2 id1,id2,nlines,n,ln,soln
      character*80 qstr
      integer*2 iconlev,jct
      logical*2 kbit
      integer*2 constraint_pos(sta_bit_words), &
     &          constraint_vel(sta_bit_words), &
     &          num_masters
      character*8 master_list(max_sta),cand_site
      integer*2 ifound,icle_ct,icle_len,ierr,outlu,trimlen
      integer*2 cle_p_levs(max_sta),cle_v_levs(max_sta)
      character*8 cle_sites(max_sta)
      logical*2 loop,except_found
      character*63 cle_file
      character*12 ref_ep_buf
      integer*2 icond_date(2), iexpand_date(3)
      integer*2 idir, idetail, jerr
      character*255 message
      integer*2 iwant,partypes(max_par),parms_wanted(1+(max_par-1)/16)
      integer*2 iproblem
      character*3 units
      character*1 pt_id
      INTEGER*4  IOS
      integer*2 iparm
      character*12 date_print
      integer*2 icpl_ct, icpl_epoch
      logical*2 is_cpl_site
      character*8 prev_sta
      character*1 prev_component
      integer*2 idummy1(sta_bit_words),idummy2(max_sta),idummy3
!
!     Place the reference epoch in the desired format,
!     yr:doy:00000.  The reference epoch is carried in
!     common as year, month and day, so the month and
!     day must be converted day of year.
!
      ref_ep_buf = 'yr:doy:00000'
      write(ref_ep_buf(1:2),"(i2.2)") ref_ep_yr
      idir = -1
      idetail = 3
      call newcents(ref_ep_yr,iexpand_date(1))
      iexpand_date(2) = ref_ep_mn
      iexpand_date(3) = ref_ep_dy
#ifdef LINUX
!cout call dt_int_ext(idir,icond_date,idetail,iexpand_date, &
!cout&  message,jerr)
#else
!cout call dt_int_ext(idir,icond_date,idetail,iexpand_date, &
!cout&  message,jerr)
#endif
      write(ref_ep_buf(4:6),"(i3.3)") icond_date(2)
!
!     Do a first pass through the covariance file (CVRFxx) to determine
!     parameter types and list sites for determining constraint levels.
!
      open(87,file=cvrf_path(1:ilen))
      read(87,*)id1,id2,npar
!     Find blank line before first parameter line
      loop = .true.
      do while (loop)
        read (87,'(A)') buf
        if (trimlen(buf).eq.0) loop = .false.
      end do
      num_masters = 0
      do i = 1,npar
        read(87,'(a)') buffer
!
!       Determine the parameter type.
!
        call parminfo(buffer,covs_wanted,partypes(i),iwant)
        call sbit(parms_wanted,i,iwant)
!
!       If the parameter is a site parameter, add it to a master site list
!       for the purpose of later determining the sites' constraints.
!
        if (partypes(i).eq.1.or.partypes(i).eq.2) then !site pos or vel
          read(buffer,'(7x,a8)') cand_site
          ifound = 0
          do j =1,num_masters
            if (cand_site.eq.master_list(j)) ifound = j
          end do
          if (ifound .eq. 0) then
            num_masters = num_masters + 1
            master_list(num_masters) = cand_site
          end if
        endif
      end do
!out  close(87)
      rewind(87)
!
!     develop position and velocity bit arrays of constraint statuses for
!     each site in the master list
!
      call cons_levs(control_path,icontrol_len,num_masters,master_list, &
     &               constraint_pos,constraint_vel, &
     &               idummy1, idummy2, idummy3)
!
!     Determine exceptions to the general constraint levels.  These are read
!     directly from a file.
!
      icle_ct = 0
      cle_file = '/data11/snp/gsnoop/con_lev_exceptions'
      icle_len = trimlen(cle_file)
      open (93,file=cle_file(1:icle_len),iostat=ios )
      ierr = ios
      if (ierr.ne.0) then
        write(qstr,'("Error ",i5," opening file of sites ")') ierr
        call asnl(qstr)
        write(qstr,'("whose constraint levels should be set directly")')
        call asnl(qstr)
        write(qstr,'("File path is: ",A)') cle_file(1:icle_len)
        call asnl(qstr)
        write(qstr,'("Please check constraint levels in your ")')
        call asnl(qstr)
        write(qstr,'("iers_solest or iers_solestap file.")')
        call asnl(qstr)
        write(qstr,'("Any key to continue")')
        call asnl(qstr)
        call getstr_f(qstr)
      else
        loop = .true.
        do while (loop)
          read(93,"(A)",end=89) qstr
          if (qstr(1:1).ne.'#') then
            icle_ct = icle_ct + 1
            read(qstr,"(A8)") cle_sites(icle_ct)
!           Dash uses the general value for a given parameter.
            if (qstr(10:10).eq.'-') then
              cle_p_levs(icle_ct) = -1
            else
              read(qstr,"(9X,I1)") cle_p_levs(icle_ct)
            end if
            if (qstr(12:12).eq.'-') then
              cle_v_levs(icle_ct) = -1
            else
              read(qstr,"(11X,I1)") cle_v_levs(icle_ct)
            end if
          end if
        end do
 89     close(93)
      end if
!
!
      if (ap_choice) then
!       Note: CONSTRAINT used to be called APR_STD_DEV
        outlu = 90
        write(outlu,"('+SOLUTION/APRIORI')")
        write(outlu,'("*INDEX TYPE__ CODE PT SOLN REF_EPOCH___ UNIT S ", &
     &           "APRIORI_VALUE________ CONSTRAINT_")')
      else
!       Note: SIGMA used to be called STD DEV
        outlu = 88
        write(outlu,"('+SOLUTION/ESTIMATE')")
        write(outlu,'("*INDEX TYPE__ CODE PT SOLN REF_EPOCH___ UNIT S ", &
     &           "ESTIMATED_VALUE______ SIGMA______")')
      end if
!
      open(49,file=SOLVE_SAVE_DIR//'antenna.dat')
!out  open(87,file=cvrf_path(1:ilen))
!     Now go back through the covariance file, printing the information for
!     each parameter in the Sinex output format.
      read(87,*)id1,id2,npar
!     Find blank line before first parameter line
      loop = .true.
      do while (loop)
        read (87,'(A)') buf
        if (trimlen(buf).eq.0) loop = .false.
      end do
      iparm = 0
      prev_sta = '        '
      prev_component = ' '
      do i=1,npar
        read(87,'(A)') buf
        read(buf,'(I6,1X,A8,1X,A11,1X,3(1X,E22.16))') &
     &       ln,sta,ptype,sig,adj,tot
!       Set the monument name.
        iproblem = 0
        if (partypes(i).eq.1.or.partypes(i).eq.2) then
          compsta = ' '
          do while(sta.ne.compsta)
            read(49,'(A8,1X,A4)',end=50)compsta,mon
          enddo
 50       if (sta.ne.compsta) then
            iproblem = 1
            write(qstr,'("solest error: ",A8," not in antenna.dat")') &
     &        sta
            call asnl(qstr)
            mon = '?!!!'
          endif
          if (iproblem.ne.1) then
!           Determine whether or not this is a cpl site.
            is_cpl_site = .false.
            do icpl_ct = 1,ncplstat
              if (sta.eq.qcpl_stat(icpl_ct)) is_cpl_site = .true.
            enddo
          endif
        else if (partypes(i).eq.3.or.partypes(i).eq.4) then
          mon = '----'
        endif
!         Set a value that relates a set of parameters to a physical point
!         (e.g., 1 and 2 for site positions before and after an earthquake.)
!         This is only meaningful for site parameters.
          if (partypes(i).eq.1.or.partypes(i).eq.2) then
            if (is_cpl_site) then
              if (sta.ne.prev_sta) then
                icpl_epoch = 0
              else if (ptype(1:1).ne.prev_component) then
                icpl_epoch = 0
              end if
              icpl_epoch = icpl_epoch + 1
              soln = icpl_epoch
            else
              soln = 1
              if (ptype(2:2).ne.' ') soln = 2
            endif
          else if (partypes(i).eq.3.or.partypes(i).eq.4) then
            soln = 1
          endif
!         Set the parameter type
          if (partypes(i).eq.1.or.partypes(i).eq.2) then
            if (ptype(1:1).eq.'X') then
              if (ptype(3:3).eq.'V') then
                pt2 = 'VELX'
              else
                pt2 = 'STAX'
              endif
            endif
            if (ptype(1:1).eq.'Y') then
              if (ptype(3:3).eq.'V') then
                pt2 = 'VELY'
              else
                pt2 = 'STAY'
              endif
            endif
            if (ptype(1:1).eq.'Z') then
              if (ptype(3:3).eq.'V') then
                pt2 = 'VELZ'
              else
                pt2 = 'STAZ'
              endif
            endif
          else if (partypes(i).eq.3) then
            if (sta(1:1).eq.'X') then
              pt2 = 'XPO '
            else
              pt2 = 'YPO '
            endif
          else if (partypes(i).eq.4) then
            pt2 = 'UT  '
          endif
!         Set the constraint level
          if (cons_override.ge.0) then
!           Override the ordinary code for setting the
!           constraint levels, to a single level for all parameters.
!           (This is initially the only way to set constraints for the eop
!            parameters.)
            iconlev = cons_override
          else
!           Set the site constraints in the ordinary way.
!
!           First check for exceptional sites which are set directly
!           from an array read from a file earlier.
!
            except_found = .false.
            do jct = 1,icle_ct
              if (sta.eq.cle_sites(jct)) then
                except_found = .true.
                if (pt2(1:3).eq.'STA') then
                  if (cle_p_levs(jct).ne.-1) iconlev = cle_p_levs(jct)
                else
                  if (cle_v_levs(jct).ne.-1)  iconlev = cle_v_levs(jct)
                end if
              end if
            end do
!
!           If this site is not an exception, check the general constraint
!           level status arrays.
!
            if (.not. except_found) then
              do jct = 1,num_masters
                if (sta .eq. master_list(jct)) then
                  if (pt2(1:3).eq.'STA') then !station parameter
                    if (kbit(constraint_pos,jct)) then
                      iconlev = 1
                    else
                      iconlev = 2
                    end if
                  else !velocity parameter
                    if (kbit(constraint_vel,jct)) then
                      iconlev = 1
                    else
                      iconlev = 2
                    end if
                  end if
                end if
              end do
            end if
          endif !end constraint level override vs. ordinary code
!         Set the units
          if (partypes(i).eq.1.or.partypes(i).eq.2) then
            if(pt2(1:3).eq.'STA') then
              units = "m  "
            else
              units = "m/y"
            endif
          else if (partypes(i).eq.3) then
            units = "mas"
          else if (partypes(i).eq.4) then
            units = "ms "
          endif
!         Set the site's point identification.  All sites will be set to A.
!         The quantity is meaningless for parameters other than sites.
          if (partypes(i).eq.1.or.partypes(i).eq.2) then
            pt_id = 'A'
          else if (partypes(i).eq.3.or.partypes(i).eq.4) then
            pt_id = '-'
          endif
!
          sig = sqrt(sig)
          if (kbit(parms_wanted,i)) then
            iparm = iparm + 1
            if (sta_all_disp.eq.'_individual_') then
              date_print = ref_ep_buf
            else
              date_print = sta_all_disp
            endif
            write(outlu, &
     &      '(I6,1X,A4,3X,A4,2X,A1,I5,1X,A12,1X,A3,2X,I1," ", &
     &      E21.15,1X,E11.6)')iparm,pt2,mon,pt_id,soln, &
     &                        date_print,units, &
     &                        iconlev,tot,sig
          endif
        prev_sta = sta
        prev_component = ptype(1:1)
        rewind(49)
      enddo
100   continue
!out  close(87)
      close(49)
      if (ap_choice) then
        write(outlu,"('-SOLUTION/APRIORI')")
      else
        write(outlu,"('-SOLUTION/ESTIMATE')")
      end if
      end
