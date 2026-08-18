      SUBROUTINE get_control_info()
!
!     Subroutine to get information from the input batch control file
!       and put it in formal arrays in gsnoop_com.i available to all
!       output subroutines.  (Earlier use(s) of the control file were
!       placed in specific spots for sinex processing and will not be
!       redone).  K. Baver 9/19/01.
!     Stations list should be sorted befor coming here, otherwise you
!     might not be very happy with the results!
!
      IMPLICIT NONE
!
      INCLUDE 'gsnoop_com.i'
      INCLUDE 'solve.i'
!
      integer*2 iconlen,trimlen,ict,jct
      integer*2 bit_dummy1(sta_bit_words),bit_dummy2(sta_bit_words)
      integer*2 ivelgrp
!
      character*8 just_site_names(max_sta_gsnoop)
!
      iconlen = trimlen(control_name)
!
!     The site_names array contains more than just site_names: characters 9+
!         carry episodic epochs and so on.  Put the first 8 characters
!         (the actual site names) into local array just_site_names, so that
!         this can be passed to cons_levs to extract info for these sites.
!
      do ict = 1,nsite
        just_site_names(ict) = site_names(ict)(1:8)
      enddo
!
!
!     cons_levs will parse the $CONSTRAINTS and $SUPPRESSION control file
!       keywords and return a few selected values.    Variables called
!       as dummy are ones specific to other parts of gsnoop.
!       The other variables are contained in gsnoop_com.i and will
!       give information to be used throughout gsnoop.
!       All site arrays are keyed to the order of site_names.
!         constraints_velocities - $CONSTRAINTS VELOCITIES keyword
!            bit array.  Bit on means site is constrained by this keyword
!            (to the reference frame in the solution, e.g., NUVEL-1A).
!         suppression_velocity_tie - $SUPPRESSION VELOCITY_TIE keyword
!            numeric array.  0 means that the site is not tied to any other
!            site's velocity (independent).  A positive number means the site
!            is tied to all other sites with the same number.
!
      call cons_levs(control_name,iconlen, &
     &               nsite,just_site_names, &
     &               bit_dummy1, bit_dummy2, &
     &               constraints_velocities, suppression_velocity_tie, &
     &               num_suppression_velocity_tie)
!
! Identify a "master" site in each velocity tie group in
! suppression_velocity_tie.  This will just be the first site in each group,
! to make things easier.  Set up a pointer to that site, in
! first_in_velgrp.
!
!   suppression_velocity_tie(isite)
!     (site position in master list) --> number of velocity group (positive)
!                                            or 0 if independent
!   first_in_velgrp(velgrp)
!     (# of velocity group) ---> position in master site array of first site
!                                in group
!  e.g.,
!                              01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
! suppression_velocity_tie      0  0  0  2  1  0  5  0  5  0  3  0  2  3  4
!
!                  vel grp #s  v1 v2 v3 v4 v5
! first_in_velgrp               5  4 11 15  7
!    the first appearance of velocity group 1 is site 5
!    the first appearance of velocity group 2 is site 4 etc.
!
!
      do jct = 1,max_vel_grps
        first_in_velgrp(jct) = 0
      enddo
!
      do ict = 1, nsite
        if (suppression_velocity_tie(ict).ne.0) then
!         This site belongs to a velocity group. Ivelgrp gets the specific
!         group.
          ivelgrp = suppression_velocity_tie(ict)
!         See if this is the first site in this velocity group.
          if (first_in_velgrp(ivelgrp).eq.0) then
!           the first site in this velocity group hasn't been recorded yet,
!           so this must be it.
            first_in_velgrp(ivelgrp) = ict
          endif
        endif
      enddo
!
      return
      end
