      Subroutine global_info
!
!     Creates tables of reference epoch positions, velocities, and corels
!
!     Modified 93 05 07 DS Caprette HSTX.  Fixed xyz table so that velocities]
!     are used as found for each station.  They are no longer zeroed out for
!     any reason.
!     :95.05.13:jwr: Data write statement for the 'tvel' file (unit = 44) modified
!                    to write with f7.2 and f6.1 instead of f7.1 and f6.1.
!     :96.07.09:kdb: Add an extra digit to the "value" field in the cvel_<>
!                    file.
!     :97.05.07:kdb: Add plate to output for tvel file.
!     :98.12.02:kdb: Output new "horaz_" file (lu 108).
!     :99.01.07:kdb: Fix error introduced with new "horaz_" file; wrote a
!                    separator line to this file even when it wasn't selected.
!     :00.10.25:kdb: Change max_sta to max_sta_gsnoop.
!     :01.09.19:kdb: Put blurb at top of cvel_<> and tvel_<> files for the
!                    quarterly solution.
!                    Also automatically put *'s before sites whose velocities
!                      are constrained to the solution's reference frame
!                      (via $CONSTRAINTS VELOCITIES) and label sites whose
!                      velocities are tied together (label that the sites are
!                      tied to an arbitrary site from the group and precede
!                      the site names with a !.
!     :06.04.29:kdb: Fix fifteen syntax errors found for the new fortran 90 
!                    version on lyra.  All consisted of placing a comma before 
!                    the variable list in a write statement.
!                  
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'gsnoop_com.i'
!
      integer*2     i, j, m, mj
      integer*2     trimlen, ilength
!
      real*8        sum_sigs
!
      character*5   qwarn
      Character*80  qstr
      Character*89  buffout
      Character*120 Buffer(6), Blank, Header(4), Separate
      character*4  qplate(max_sta_gsnoop)
      character*8  qsite(max_sta_gsnoop)
      integer*2    lensp,ivelgrp,imaster_vel_tie
      character*8  master_vel_tie_site
      character*1  vel_symbol
      character*18 tie_blurb
      logical*2    kbit
!
      data qwarn /'     '/
!
!
      write (qstr , "('Doing global velocities tables')")
      call asnl(qstr )
      if (do_3_sig) then
      write (qstr , "('With 3-sigma errors')")
        call as2nl(qstr )
      end if
!
!
      write (blank(1:), "(120x)")
      do i= 1,120
        write (separate(i:i), "('-')")
      end do
!
      lensp = trimlen(spool_name)
!
      if (do_vel) then
        write(43,"('GSFC VLBI Solution ')")
        write(43,"(/,'Cartesian positions and velocities.')")
        write(43,"('Note: Sites preceded by * have velocities ', &
     &             'constrained to NUVEL-1A for insufficient data.')")
        write(43,"('Also, some sites with weak data were pooled with ', &
     &            'adjacent sites ')")
        write(43,"('to derive a common velocity.  Thus these ', &
     &            'sites do not represent independent information.  ')")
        write(43,"('In this file, the first site in each group of pooled ', &
     &            'or ""tied"" velocities')")
        write(43,"('is chosen as the master site, ', &
     &             'and other sites in the group are preceded by !, ')")
        write(43,"('with an identification of the ""master"" site ', &
     &             'in the lower left corner of the site''s data.')")
        write(43,"(/,'Spoolfile:  ',A)") spool_name(1:lensp)
!
        write(44,"('GSFC VLBI Solution ')")
        write(44,"(/,'Site topocentric velocities.')")
        write(44,"('Note: Sites preceded by * have velocities ', &
     &             'constrained to NUVEL-1A for insufficient data.')")
        write(44,"('Also, some sites with weak data were pooled with ', &
     &            'adjacent sites ')")
        write(44,"('to derive a common velocity.  Thus these ', &
     &            'sites do not represent independent information.  ')")
        write(44,"('In this file, the first site in each group of pooled ', &
     &            'or tied velocities')")
        write(44,"('is chosen as the master site, ', &
     &             'and other sites in the group are preceded by !, ')")
        write(44,"('with an identification of the ""master"" site ', &
     &             'below the site''s rate totals.')")
        write(44,"(/,'Spoolfile:  ',A)") spool_name(1:lensp)
!
      endif
!
!     Warning: We assume that site_names has been sorted
!     Write the header for the site velocities file
!
      if ((do_vel).and.(do_3_sig)) then
        Write(43,"('Errors in this table are 3-sigma')")
        Write(44,"('Errors in this table are 3-sigma')")
        Write(108,"('Errors in this table are 3-sigma')")
      end if
!
!
      if (do_vel) Write(44,'("|",a,"|")') Separate(1:107)
      Write(Header(1),'( &
     &"|          |      Total Rates     |   Relative to Model  ", &
     &"| Total rates | Rel. to Model |  Error Ellipsoid   |")')
      Write(Header(2),'( &
     &"| Site     |  East   North   Up   | East    North    Up  ", &
     &"| Hor    Az   |  Hor     Az   | Amp    Az    Elev  |")')
      ilength = trimlen(header(1))
      if (do_vel) Write(44,'(a)') Header(1)(1:ilength)
      ilength = trimlen(header(2))
      if (do_vel) Write(44,'(a)') Header(2)(1:ilength)
!
      Write(Header(1),'( &
     &"|          |  mm/yr  mm/yr  mm/yr | mm/yr   mm/yr   mm/yr", &
     &"| mm/yr  deg  |  mm/yr   deg  | mm/yr  deg    deg  |")')
      ilength = trimlen(header(1))
      if (do_vel) Write(44,'(a)') header(1)(1:ilength)
!
      if (do_vel) Write(44,'("|",a,"|")') Separate(1:107)
      Write(Header(1),'( &
     &"| Name     |  Value  Value  Value | Value   Value  Value ", &
     &"| Value Value |  Value  Value |                    |")')
      Write(Header(2),'( &
     &"| Monument |  Sigma  Sigma  Sigma |                      ", &
     &"| Sigma Sigma |  Sigma  Sigma |                    |")')
      ilength = trimlen(header(1))
      if (do_vel) Write(44,'(a)') Header(1)(1:ilength)
      ilength = trimlen(header(2))
      if (do_vel) Write(44,'(a)') Header(2)(1:ilength)
!
      if (do_vel) Write(108,'("|",a,"|")') Separate(1:92)
      Write(Header(1),'( &
     &"|          |  Longitude   Latitude ", &
     &"|       Total rates        |        Rel. to Model         |")')
      Write(Header(2),'( &
     &"|   Site   |                       ", &
     &"| Hor (mm/yr)    Az (deg)  |  Hor (mm/yr)      Az (deg)   |")')
      Write(Header(3),'( &
     &"|   Name   |                       ", &
     &"| value sigma  value sigma |  value  sigma   value  sigma |")')
      ilength = trimlen(header(1))
      if (do_vel) Write(108,'(a)') Header(1)(1:ilength)
      ilength = trimlen(header(2))
      if (do_vel) Write(108,'(a)') Header(2)(1:ilength)
      ilength = trimlen(header(3))
      if (do_vel) Write(108,'(a)') header(3)(1:ilength)
!
      if (do_vel) then
          Write(52,'(t29,"U",t41,"E",t53,"N",t65,"U sig",t77,"E sig", &
     &           t89, "N sig")')
      end if
!
      if (do_3_sig) then
          Write(52,"('Errors in this table are 3-sigma')")
      end if
!
!     Write the header for the flyby file
!
      if (site_flyby) then
         Write(50,'("$$",/,"$$", &
     &   /,"$$ VLBI Site positions  ", &
     &   /,"$$  positions:     x (m)          y (m)          z (m)")')
!
         write(63, "('$$'/ &
     &    '$$  velocities:    x (mm/yr)       y (mm/yr)      z (mm/yr)'/ &
     &    '$$')")
       end if
!
!    Write headers for episodic site xyz positions file
!
        if (do_ep_sites) then   !write headers
          Write (54,'(//,20x," VLBI episodic site positions"/)')
          Write(54,'("  Site   Mon.   Epoch   ", &
     &          14x ,"X",6x,"Error", &
     &          13x ,"Y",6x,"Error", &
     &          13x ,"Z",6x,"Error",/, &
     &          36x ,"(mm)",7x,"(mm) ", &
     &          11x ,"(mm)",7x,"(mm) ", &
     &          11x ,"(mm)",7x,"(mm)",/)')
        end if
!
      Do I=1,nsite
!
!
!       If a group of sites' velocities are tied together, in reality they are
!       equally tied with no primary site.  But for reporting purposes, this
!       subroutine will identify a "master" site and act as if the other sites
!       were tied to this master site.  The "tied" sites will be preceded by a
!       !, and the name of the "master" site will be printed in the lower
!       left corner of the tied site's information box.  No special markings
!       will be made for the "master" site's information box.
!
!       The first step in all of this is to determine if the current site is:
!          a "master" site (set master_vel_tie_site to !!!!!!!!)
!          a "tied" site   (set master_vel_tie_site to the "master"'s name.
!          not in a velocity tie group at all (set master_veL_tie_site to
!                                               --------)
        if (suppression_velocity_tie(i).ne.0) then
!         This site is in a velocity tie group.  Determine the site to which
!         it is tied.  (For this purpose, the first site entered in the
!         data array is used arbitrarily..)
          ivelgrp = suppression_velocity_tie(i)
          imaster_vel_tie = first_in_velgrp(ivelgrp)
          if (imaster_vel_tie.ne.i) then
!           This is the 2nd+ site in its velocity tie group.
            if (site_names(imaster_vel_tie)(1:8).eq.site_names(i)(1:8)) then
!             The master site is episodic, and this is its 2+ occurrence in
!             the site list.  Record that this is still the master site.
              master_vel_tie_site = '!!!!!!!!'
            else
!             This is truly a different site than the first or "master" site,
!             so it's considered tied to the "master" site, and that master
!             site should be recorded now.
              master_vel_tie_site = site_names(imaster_vel_tie)(1:8)
            endif
          else
!           This is the first site in its velocity tie group, so it's the
!           master.
            master_vel_tie_site = '!!!!!!!!'
          endif
        else
!         not in velocity tie group
          master_vel_tie_site = '--------'
        endif
!
!       Do the xyz global_info file
!
        Write(buffout(1:1),   '("|")')
        Write(buffout(89:89), '("|")')
        Do J = 1,6
          Buffer(J) = Blank
        Enddo
        if (kdebug) then
          write (qstr ,"(i3)") i
          call asnl(qstr )
        end if
        Read(site_names(I)(19:23),'(I5)') m
        if (kdebug) then
          write (qstr ,"(a)") site_names(i)
          call asnl(qstr )
        end if
        Write(buffout(2:88),'(a)') Separate(1:87)
        if (do_vel) Write(43,'(a)') buffout(1:89)
        Write(buffout(2:), &
     &    '(2x,a8,4x,"Coordinate Reference Epoch = ",3(i2.2,x))') &
     &    site_names(I)(1:8), (irefdate(j,m), j=1,3)
!
!       if the site velocity was constrained to the reference frame via
!       $CONSTRAINTS VELOCITIES, indicate this by preceding the site name with
!       a *.
!
        if (kbit(constraints_velocities,i)) buffout(2:2) = '*'
!
!       If the site's velocity was tied to another master site,
!        precede the site name with !.  (Note: the master site itself is not
!        flagged.)
!
        if (master_vel_tie_site.ne.'--------'.and. &
     &      master_vel_tie_site.ne. &
     &        '!!!!!!!!')buffout(2:2) = '!'
!
        if (site_names(i)(13:18) .ne. &
     &  "000000")Write(buffout(56:), &
     &  "('Episodic date:'3(x,a2))")site_names(i)(13:14), site_names(i)(15:16), site_names(i)(17:18)
!
        Write(buffout(89:89), '("|")')
        if (do_vel) Write(43,'(a)') buffout(1:89)
!
        Write(buffout(2:), &
     &        '(4x,a4,t22," Value   Error", &
     &        t41,"Correlation Matrix:")') &
     &        site_names(i)(9:12)
        Write(buffout(89:89), '("|")')
        if (do_vel) Write(43,'(a)') buffout(1:89)
        Write(Buffer(1),1)  'X     (mm)', xyz(1,m), xyz_sig(1,m)*scale
        Write(Buffer(2),1)  'Y     (mm)', xyz(2,m), xyz_sig(2,m)*scale
        Write(Buffer(3),1)  'Z     (mm)', xyz(3,m), xyz_sig(3,m)*scale
!
!       Use velocities for 'm' entry if any are nonzero.  Otherwise
!       look for nonzero velocities for same station at a different
!       epoch and use those.
!
        if  ((dabs(xyz_dot(1,m)) .gt. 0.0001) .or. &
     &       (dabs(xyz_dot(2,m)) .gt. 0.0001) .or. &
     &       (dabs(xyz_dot(3,m)) .gt. &
     &  0.0001))then
          Write(Buffer(4),3)  &
     &            'X vel (mm/yr)', xyz_dot(1,m), xyz_dot_sig(1,m)*scale
          Write(Buffer(5),3)  &
     &            'Y vel (mm/yr)', xyz_dot(2,m), xyz_dot_sig(2,m)*scale
          Write(Buffer(6),3)  &
     &            'Z vel (mm/yr)', xyz_dot(3,m), xyz_dot_sig(3,m)*scale
!
        else !Find nonzero velocities for some other entry for same site.
          j = 1
          Read(site_names(j)(19:23),'(I5)') mj
          do while  ((j .le. nsite) .and. &
     &              ((site_names(j)(1:8) .ne. site_names(i)(1:8)) .or. &
     &              ((dabs(xyz_dot(1,mj)) .lt. 0.0001) .and. &
     &               (dabs(xyz_dot(2,mj)) .lt. 0.0001) .and. &
     &               (dabs(xyz_dot(3,mj)) .lt. 0.0001))))
            j = j + 1
!           if statement required so read won't blow up at end of list
            if (j .le. nsite) Read(site_names(j)(19:23),'(I5)') mj
          end do
          if (j .le. nsite) then !nonzero velocities were found
            Read(site_names(j)(19:23),'(I5)') mj
!
!
            Write(Buffer(4),3)  &
     &          'X vel (mm/yr)', xyz_dot(1,mj), xyz_dot_sig(1,mj)*scale
            Write(Buffer(5),3)  &
     &          'Y vel (mm/yr)', xyz_dot(2,mj), xyz_dot_sig(2,mj)*scale
            Write(Buffer(6),3)  &
     &          'Z vel (mm/yr)', xyz_dot(3,mj), xyz_dot_sig(3,mj)*scale
!
          else !Nonzero velocities NOT found so use the zero ones.  But warn
!               user just in case.
                write (qstr , &
     &                         "('All velocities at ',a,' are zero!')")Site_names(I)
                call asnl(qstr )
            Write(Buffer(4),3)  &
     &            'X vel (mm/yr)', xyz_dot(1,m), xyz_dot_sig(1,m)*scale
            Write(Buffer(5),3)  &
     &            'Y vel (mm/yr)', xyz_dot(2,m), xyz_dot_sig(2,m)*scale
            Write(Buffer(6),3)  &
     &            'Z vel (mm/yr)', xyz_dot(3,m), xyz_dot_sig(3,m)*scale
          end if
        end if
!
    1   Format(a10,f17.2,x,f7.1)
    3   Format(a13,f14.2,x,f7.1)
!
!       If the site's velocity was tied to another "master" site, put a *
!       after the x velocity label to warn the user to look for the identity
!       of the master site in the lower left corner of this site's information
!       box.
!
        if (master_vel_tie_site.ne.'--------'.and. &
     &      master_vel_tie_site.ne.'!!!!!!!!') then
              write(buffer(4)(14:14),"('*')")
              write(buffer(5)(14:14),"('*')")
              write(buffer(6)(14:14),"('*')")
        endif
!
!       Correlations
!
        Write(Buffer(1)(37:),2) 1.d0
        Write(Buffer(2)(37:),2) y_x (m),1.d0
        Write(Buffer(3)(37:),2) z_x (m),z_y (m),1.d0
        Write(Buffer(4)(37:),2) xd_x(m),xd_y(m),xd_z(m),1.d0
        Write(Buffer(5)(37:),2) yd_x(m),yd_y(m),yd_z(m),yd_xd(m),1.d0
        Write(Buffer(6)(37:),2) zd_x(m),zd_y(m),zd_z(m),zd_xd(m), &
     &                           zd_yd(m),1.d0
!
    2   Format(6f8.3)
!
        Write(buffout(1:3), '("|  ")')
        Do J=1,6
          ilength = trimlen(buffer(J))
          Write(buffout(4:),'(a)')Buffer(J)(1:ilength)
          Write(buffout(89:89), '("|")')
          if (do_vel) Write(43,'(a)') buffout(1:89)
        Enddo
!
        Write(buffout(2:), &
     &          '(41x,"X       Y       Z      X vel   Y vel   Z vel")')
        Write(buffout(89:89), '("|")')
!
!       We're about to write the final line of the information box.
!       If this site was "tied" to a "master" site, write the name of that
!       site out now at the start of the line (which will be the lower left
!       corner of the site's information box).
        if (master_vel_tie_site.ne.'--------'.and. &
     &      master_vel_tie_site.ne.'!!!!!!!!') &
     &   write(buffout(5:24), &
     &     "(' * tied to ',A8,' ')")master_vel_tie_site
        if (do_vel) Write(43,'(a)') buffout(1:89)
!
!       Do the eun adjustments
!
        if (do_vel) Write(52,'(a8,2x,a4,x,a6,6(2x,f10.3))') &
     &   site_names(I)(1:8), site_names(I)(9:12), site_names(I)(13:18), &
     &   (uen(j,m),j=1,3), (uen_sig(j,m)*scale,j=1,3)
!
!       Do the episodic sites.
!
        if ((do_ep_sites).and.(site_names(i)(13:18) .ne. "000000")) then
          Write(54, &
     &      '(1X,A8,1X,A4,1X,3(a2,x),3(F17.1,F8.1))')SITE_NAMES(I)(1:8), SITE_NAMES(I)(9:12), &
     &      SITE_NAMES(I)(13:18), (xyz(j,m), xyz_sig(j,m)*scale,j=1,3)
!
           Write(55,'(a8,2x,a4,x,3(a2,x),6(2x,f10.3))') &
     &       site_names(I)(1:8), SITE_NAMES(I)(9:12), &
     &       SITE_NAMES(I)(13:14), SITE_NAMES(I)(15:16), &
     &       SITE_NAMES(I)(17:18), &
     &       (uen(j,m),j=1,3), (uen_sig(j,m)*scale,j=1,3)
        end if
!
!       Do the horizontal velocities and error ellipse
!
      if ((dabs(xyz_dot(1,m)) .gt. 0.0001) .or. &
     &                  (dabs(xyz_dot(2,m)) .gt. 0.0001) .or. &
     &                  (dabs(xyz_dot(3,m)) .gt. &
     &  0.0001))then
          if (do_vel) Write(44,'("|",a,"|")') Separate(1:107)
          if (master_vel_tie_site.ne.'--------'.and. &
     &        master_vel_tie_site.ne.'!!!!!!!!') then
!           If this site's velocity was tied to another site's,
!           precede the site name with !
            vel_symbol = '!'
          else if (kbit(constraints_velocities,i)) then
!           If this site's velocity was constrained to the reference frame via
!           $CONSTRAINTS VELOCITIES, preced the site name with *.
            vel_symbol = '*'
          else
            vel_symbol = ' '
          endif
          if (do_vel) Write(44,'( &
     &    "|",a1,a8,1X,"|",3f7.2, &
     &    " |", 3f7.2," |",f6.2,f6.1, &
     &    " |",f7.2,f7.1," |", f6.2,f7.1,f6.1," |")') &
     &    vel_symbol, &
     &    site_names(I)(1:8), &
     &    uen_dot(2,m),        uen_dot(3,m),       uen_dot(1,m), &
     &    Delta_east_vel(m),   Delta_north_vel(m), Delta_up_vel(m), &
     &    Hor_vel(m),          Hor_az(m), &
     &    Hor_vel_adj(m),      Hor_az_adj(m), &
     &    Error_amp(1,m),      Error_az(1,m),      Error_el(1,m)
!
          if (do_vel) Write(44,'( &
     &     "|",3x,a4,3x,"|",3f7.2, &
     &    " |",21x," |",f6.2,f6.1, &
     &    " |",f7.2,f7.1," |",f6.2,f7.1,f6.1," |")') &
     &    site_names(i)(9:12), &
     &    uen_dot_sig(2,m)*scale,    uen_dot_sig(3,m)*scale, &
     &    uen_dot_sig(1,m)*scale,    Hor_vel_sig(m)*scale, &
     &    Hor_az_sig(m)*scale,       Hor_vel_adj_sig(m)*scale, &
     &    Hor_az_adj_sig(m)*scale,   Error_amp(2,m), &
     &    Error_az(2,m),             Error_el(2,m)
!
        if (do_vel) then
          qsite(1) = site_names(i)(1:8)
          call qsitpl( qsite, INT2((1)), qplate )
        endif
!
        if (master_vel_tie_site .eq. '!!!!!!!!' .or. &
     &      master_vel_tie_site .eq. '--------') then
          write(tie_blurb,"('         ',8x,' ')")
        else
          write(tie_blurb,"('(tied to ',a8,')')") master_vel_tie_site
        endif
        if (site_names(i)(13:18) .ne. "000000") then
          if (do_vel) then
            Write(44,'("|",2x,a6,2x,"|",2x,a18,2x,"|", &
     &         9x,a4,8x," |",12x," |",15x, &
     &                  "|",f6.2,f7.1,f6.1," |")') &
     &      site_names(i)(13:18), &
     &      tie_blurb, &
     &      qplate(1), &
     &      Error_amp(3,m),      Error_az(3,m),      Error_el(3,m)
          end if
        else
          if (do_vel) then
            Write(44,'("|",10x,"|",2x,a18,2x,"|", &
     &         9x,a4,8x," |",12x," |",15x,"|", &
     &      f6.2,f7.1,f6.1," |")') &
     &      tie_blurb, &
     &      qplate(1), &
     &      Error_amp(3,m),      Error_az(3,m),      Error_el(3,m)
          end if
        end if
!
          if (do_vel) Write(108,'("|",a,"|")') Separate(1:92)
          if (do_vel) Write(108,'( &
     &    "|",x,a8,1X,"|",2f11.4, &
     &    " |", f6.2,f6.2,1X,f6.1,f6.1, &
     &    " |",f7.2,f7.2,1X,f7.1,f7.1," |")') &
     &    site_names(I)(1:8), &
     &    site_lon(m), site_phi(m), &
     &    Hor_vel(m),           Hor_vel_sig(m)*scale, &
     &    Hor_az(m),            Hor_az_sig(m)*scale, &
     &    Hor_vel_adj(m),       Hor_vel_adj_sig(m)*scale, &
     &    Hor_az_adj(m),        Hor_az_adj_sig(m)*scale
!
      end if
!
!       Do the flyby positions
!
 1011   format (4X, a8, f15.3, x, 2f15.3, 2x, 3(x,a2))
        if (site_flyby) write(50, &
     &    1011)SITE_NAMES(I)(1:8), (xyz(j,m)/1.0d3, j=1,3), &   !convert from mm to m
     &    SITE_NAMES(I)(13:14), SITE_NAMES(I)(15:16), &
     &    SITE_NAMES(I)(17:18)
!
!       Do the cmpar positions
!
 1013 format (14X, a8, 1x, a4, 2x, 3(f22.3,f7.3))
        if (do_cmpar) write(75, &
     &   1013)SITE_NAMES(I)(1:8), SITE_NAMES(I)(9:13), &
     &   (xyz(j,m), xyz_sig(j,m), j=1,3)
!
      Enddo
!
!     Finish up with the flyby velocities
!
 1012 format (4X, a8, f15.1, x, 2f15.1, 2x, a)
!
      if (site_flyby .or. do_cmpar) then
!
!
        Do I=1,nsite
          if (site_names(i)(13:18) .eq. "000000") then
            Read(SITE_NAMES(I)(19:23),'(I5)') m
            sum_sigs = xyz_dot_sig(1,m) + xyz_dot_sig(2, &
     &                                  m)+ xyz_dot_sig(3,m)
            if (sum_sigs .gt. 0.0d0) then !velocities were adjusted
              qwarn = "     "
              if (do_cmpar) then
                write(76, &
     &          1013)SITE_NAMES(I)(1:8), SITE_NAMES(I)(9:13), &
     &          (xyz_dot(j,m), xyz_dot_sig(j,m), j=1,3)
!
                write(77, &
     &          1013)SITE_NAMES(I)(1:8), SITE_NAMES(I)(9:13), &
     &          (uen_dot(j,m), uen_dot_sig(j,m), j=1,3)
              end if
            else                !model velocities were used
              qwarn = "MODEL"
            end if
!
!           Use velocities for 'm' entry if any are nonzero.
            if ((dabs(xyz_dot(1,m)) .gt. 0.0001) .or. &
     &          (dabs(xyz_dot(2,m)) .gt. 0.0001) .or. &
     &          (dabs(xyz_dot(3,m)) .gt. &
     &      0.0001))then
              write(63, 1012)  &
     &          SITE_NAMES(I)(1:8), &
     &          (xyz_dot(j,m), j=1,3), qwarn  !leave in mm/yr
!
            else !Find nonzero velocities for some other entry for same site.
              j = 1
              do while (((site_names(j)(1:8) .ne. &
     &                  site_names(i)(1:8)).or.  ((dabs(xyz_dot(1,m)) .gt. &
     &                         0.0001) .and.(dabs(xyz_dot(2,m)) .gt. 0.0001) &
     &                         .and.(dabs(xyz_dot(3,m)) .gt. &
     &                  0.0001))).and.             (j .le. nsite))
                j = j + 1
              end do
!
              if (j .le. nsite) then !nonzero velocities were found
                Read(site_names(j)(19:23),'(I5)') mj
                write(63, 1012)  &
     &            SITE_NAMES(I)(1:8), &
     &            (xyz_dot(j,mj), j=1,3), qwarn  !leave in mm/yr
!
              else !Nonzero velocities were NOT found so use 'm' entry anyway.
                write (qstr , &
     &                         "('All velocities at ',a,' are zero!')")Site_names(I)
                call asnl(qstr )
                write(63, 1012)  &
     &            SITE_NAMES(I)(1:8), &
     &            (xyz_dot(j,m), j=1,3), qwarn  !leave in mm/yr
!
              end if
            end if
          end if
        end do  ! i = 1,nsite
      end if  !(site_flyby)
!
!     Finish out files
      Write(buffout(2:88),'(a)') Separate(1:87)
      if (do_vel) then
        Write(43,'(a)') buffout(1:89)
        Write(44,'("|",a,"|")') Separate(1:107)
        Write(108,'("|",a,"|")') Separate(1:92)
      end if
      if (site_flyby) Write(50, "('$$')")
      if (site_flyby) Write(63, "('$$')")
!
      Return
      END
