      SUBROUTINE topo_bin()
!
!     PROGRAM TO
!
!     modifications:
!
!     kdb   2/6/95 update error msg from "site not found in list" to
!                  identify file in which site does not exist.
!     kdb 11/20/95 Add new file, lu 98, as "uen_sigs_<>".
!     kdb  2/27/98 Move antenna.dat to solve files directory for the benefit
!                  of solve.
!     kdb 10/25/00 Parameterize antenna.dat directory.
!
      IMPLICIT NONE
      INCLUDE 'gsnoop_com.i'
      INCLUDE 'solve.i'
!
      integer*2 i, j, k, n, m
      INTEGER*4 ios
      CHARACTER*1   qans
      character*5   quen          !East, North, Up
      character*6   qtype         !Fixed, Mobile
      character*15  qbuffer
      character*80  qstr
      character*80 afile
!
      integer*2  ibin_as(3,3,8)           !uen_adjustment sigma bins
      integer*2  ibin_vs(3,3,8)           !uen_velocity sigma bins
      integer*2  tot_as(3,3), tot_vs(3,3) !total # sigmas binned
      real*8     rss_en,rss_dot_en        !rss of a site's east and north
!sigmas.  Position and velocity.
      real*8     rss_all,rss_dot_all      !rss of all three of a  site's
!sigmas.  Position and velocity.
      integer*2  ict
!
!           i:    1 = Fixed antenna
!                 2 = Mobile or transportable antenna
!                 2 = Unknown type of antenna
!
!           j:    1 = up adjustment or velocity
!                 2 = east adjustment or velocity
!                 3 = north adjustment or velocity
!
!           k:    1 =  0 < sigma < 1 mm or mm/yr
!                 2 =  1 < sigma < 2 mm or mm/yr
!                 3 =  2 < sigma < 3 mm or mm/yr
!                 4 =  3 < sigma < 4 mm or mm/yr
!                 5 =  4 < sigma < 5 mm or mm/yr
!                 6 =  5 < sigma < 10 mm or mm/yr
!                 7 = 10 < sigma < 20 mm or mm/yr
!                 8 = 20 < sigma      mm or mm/yr
!
      write (qstr ,"('Binning the sigmas.')")
      call asnl(qstr)
      write (53,"('topocentric adjustment and velocity histograms'/)")
!
!     Write headers for uen_sigs file.
!
      write(98,"(10x, &
     &  '                    Position Sigmas                    ', &
     &  '                    Velocity Sigmas                    ')")
      write(98,"(12x, &
     &  '    Up        East       North       RSS        RSS    ', &
     &  '    Up        East       North       RSS        RSS    ')")
      write(98,"(12x, &
     &  '                                     E&N        all    ', &
     &  '                                     E&N        all    ')")
!
!     Initialize bins
!
      do i = 1,3
        do j = 1,3
          tot_as(i,j) = 0
          tot_vs(i,j) = 0
          do k = 1,8
            ibin_as(i,j,k) = 0
            ibin_vs(i,j,k) = 0
          end do
        end do
      end do
!
      qbuffer = "xxxxxxxxxxxxxxx"
!
      afile = SOLVE_SAVE_DIR//'antenna.dat'
      open (49,file=afile)
!
!     Main loop.
!
      do n = 1, nsite
!
!       skip post quake stuff
!
        if (site_names(n)(13:18) .eq. "000000") then
!
!         Check antenna.dat to find if fixed or mobile
!         1 = fixed
!         2 = mobile
!
          rewind (49)
          do while (site_names(n)(1:8) .ne. qbuffer(1:8))
            read (49, "(a)", end=910, iostat=ios) qbuffer
          end do
!
          qans = qbuffer(15:15)
          if ((qans .eq. "F").or.(qans .eq. "f")) then
            i = 1
          else if ((qans .eq. "M").or.(qans .eq. "m")) then
            i = 2
          else
            i = 3
          end if
!
 910      if (ios .ne. 0) then
            write (qstr , "('station ',a8,' not found in: ')") &
     &              site_names(n)(1:8)
            call asnl(qstr)
            write (qstr, "(A)") afile
            call asnl(qstr)
            write (qstr , "('Please enter (F)ixed or (M)obile')")
            call asnl(qstr)
            call getstr_f(qstr)
            qans = qstr(1:1)
            if ((qans .eq. "F").or.(qans .eq. "f")) then
              i = 1    !fixed
            else if ((qans .eq. "M").or.(qans .eq. "m")) then
              i = 2    !mobile
            else
              i = 3    !bizarre
            end if
          end if
!
!
!         get uen adjustment and velocity sigmas
!         and put in proper bins
!
          read(site_names(n)(19:23),'(I5)') m
!
          do j = 1,3    !1=up, 2=east, 3=north
!
            tot_as(i,j) = tot_as(i,j) + 1
            if (uen_sig(j,m) .lt. 1.0d-6) then
              tot_as(i,j) = tot_as(i,j) - 1
            else if (uen_sig(j,m) .lt. 1.0d0) then
              ibin_as(i,j,1) = ibin_as(i,j,1) + 1
            else if (uen_sig(j,m) .lt. 2.0d0) then
              ibin_as(i,j,2) = ibin_as(i,j,2) + 1
            else if (uen_sig(j,m) .lt. 3.0d0) then
              ibin_as(i,j,3) = ibin_as(i,j,3) + 1
            else if (uen_sig(j,m) .lt. 4.0d0) then
              ibin_as(i,j,4) = ibin_as(i,j,4) + 1
            else if (uen_sig(j,m) .lt. 5.0d0) then
              ibin_as(i,j,5) = ibin_as(i,j,5) + 1
            else if (uen_sig(j,m) .lt. 1.0d1) then
              ibin_as(i,j,6) = ibin_as(i,j,6) + 1
            else if (uen_sig(j,m) .lt. 2.0d1) then
              ibin_as(i,j,7) = ibin_as(i,j,7) + 1
            else if (uen_sig(j,m) .ge. 2.0d1) then
              ibin_as(i,j,8) = ibin_as(i,j,8) + 1
            end if
!
            tot_vs(i,j) = tot_vs(i,j) + 1
            if (uen_dot_sig(j,m) .lt. 1.0d-6) then
              tot_vs(i,j) = tot_vs(i,j) - 1
            else if (uen_dot_sig(j,m) .lt. 1.0d0) then
              ibin_vs(i,j,1) = ibin_vs(i,j,1) + 1
            else if (uen_dot_sig(j,m) .lt. 2.0d0) then
              ibin_vs(i,j,2) = ibin_vs(i,j,2) + 1
            else if (uen_dot_sig(j,m) .lt. 3.0d0) then
              ibin_vs(i,j,3) = ibin_vs(i,j,3) + 1
            else if (uen_dot_sig(j,m) .lt. 4.0d0) then
              ibin_vs(i,j,4) = ibin_vs(i,j,4) + 1
            else if (uen_dot_sig(j,m) .lt. 5.0d0) then
              ibin_vs(i,j,5) = ibin_vs(i,j,5) + 1
            else if (uen_dot_sig(j,m) .lt. 1.0d1) then
              ibin_vs(i,j,6) = ibin_vs(i,j,6) + 1
            else if (uen_dot_sig(j,m) .lt. 2.0d1) then
              ibin_vs(i,j,7) = ibin_vs(i,j,7) + 1
            else if (uen_dot_sig(j,m) .ge. 2.0d1) then
              ibin_vs(i,j,8) = ibin_vs(i,j,8) + 1
            end if
!
          end do ! j=1,3
!
!         write a line to the uen_sigs_<> file for this site.
!         (This file has u,e,n sigmas and the rss's of the e and n and of all
!          sigmas for positions and velocities.)
!
!         Calculate:
!           rss of east and north position sigmas
!           rss of all three position sigmas
!           rss of east and north velocity sigmas
!           rss of all three velocity sigmas
          rss_en  = dsqrt(uen_sig(2,m)**2 + uen_sig(3,m)**2)
          rss_all = &
     &      dsqrt(uen_sig(1,m)**2 + uen_sig(2,m)**2 + uen_sig(3,m)**2)
          rss_dot_en  = dsqrt(uen_dot_sig(2,m)**2 + uen_dot_sig(3,m)**2)
          rss_dot_all = &
     &      dsqrt(uen_dot_sig(1,m)**2 + uen_dot_sig(2,m)**2 + &
     &            uen_dot_sig(3,m)**2)
          write(98,"(a8,2x,10(F10.3,1X))") &
     &      site_names(n)(1:8), &
     &      (uen_sig(ict,m),ict=1,3),rss_en,rss_all, &
     &      (uen_dot_sig(ict,m),ict=1,3),rss_dot_en,rss_dot_all
!
        end if !  .ne. 'ep'
      end do ! n = 1, nsite
!
!     Write output to file
!
      write (53, &
     &"('bins',31x,'<1  1-2  2-3  3-4  4-5 5-10 10-20 >20  tot'/)")
!
      do j = 1,3
        do i = 1,3
          if (j .eq. 1) then
            quen = 'up   '
          else if (j .eq. 2) then
            quen = 'east '
          else if (j .eq. 3) then
            quen = 'north'
          end if
!
          if (i .eq. 1) then
            qtype = 'Fixed '
          else if (i .eq. 2) then
            qtype = 'Mobile'
          else if (i .eq. 3) then
            qtype = 'Unknow'
          end if
!
      write (53,"(a6, ' adjustments error ',a5,' :',9(3x,i2))") &
     &      qtype, quen, (ibin_as(i,j,k),k=1,8), tot_as(i,j)
!
        end do
      end do
!
      write (53,"( )")
!
      do j = 1,3
        do i = 1,3
          if (j .eq. 1) then
            quen = 'up   '
          else if (j .eq. 2) then
            quen = 'east '
          else if (j .eq. 3) then
            quen = 'north'
          end if
!
          if (i .eq. 1) then
            qtype = 'Fixed '
          else if (i .eq. 2) then
            qtype = 'Mobile'
          else if (i .eq. 3) then
            qtype = 'Unknow'
          end if
!
      write (53,"(a6, ' velocity error    ',a5,' :',9(3x,i2))") &
     &      qtype, quen, (ibin_vs(i,j,k),k=1,8), tot_vs(i,j)
!
        end do
      end do
!
      close (53)
      close (49)
!
      return
 911  write (qstr ,"('error exit ( 0 ) from topo_bin subroutine',i5)") ios
      call asnl(qstr)
      return
      end
