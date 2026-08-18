      SUBROUTINE get_glob_source()
!
!     modifications:
!
!      1/19/95 kdb One more digit of precision in the RA and Declination value
!                  and uncertainty fields.
!                  Handle change in correlation spool file line.
!      2/27/95 kdb If RA uncertainty not in spool file, print value of
!                  9.9999999 not 9.9999993 in IERS source file.
!                  If declination uncertainty not in spool file, print value of
!                  9.999999, not 9.999990 in IERS source file.
!      9/29/95 kdb Fix inability to read 2+ sources from global section of
!                  spoolfile.  (Apparently an extra blank line was added at some
!                  point after each set of rt. asc., dec. and correlation
!                  lines.)
!     10/16/95 kdb One more digit of precision in RA and declination seconds
!                  and uncertainty fields AND in ra/dec correlation.
!                  (Handling by changing to free format reads, to minimize
!                  future updates.)
!                  Fix inability to handle old spoolfile source lines with
!                  rt. asc. starting in column 16 (ipos = 1 case).
!      3/18/96 kdb Trap error in reading declination position seconds field in
!                  source line of type 2.
!      2/6/98  kdb Fix error in which the sign of low declination Southern
!                  sources is not printed.   (Don't read the negative sign
!                  into a specific field -- the leading field may be zero,
!                  so the negative may be lost or hard to locate (field 1 vs.
!                  2).  Instead, parse the fields for a negative sign and
!                  place it in a separate variable.)
!     02/05/15 kdb The parameter index in solve spoolfile adjustments is
!                  being enlarged from a four character field to five
!                  characters, and to keep things aligned, the source
!                  coordinate correlations are being moved.  Update to read
!                  the new formats.
!     03/01/09 kdb Change from separate changeover variables to an array.
!                  Add julian date changeover array to common.
!     03/01/09 kdb Update to a change in the spoolfile format made in
!                  the 2002.12.26 solve release.  The changes adds a reference
!                  date to the source correlation line, so this
!                  subroutine can no longer read to the end of the line to
!                  capture the now middle of line ra/dec correlation field.
!     03/03/24 kdb Extract source right ascension and declination adjustments
!                  from spoolfile and place in common.
!     04/11/17 kdb Source proper motions are now estimated.  These lines must be
!                  skipped.
!
      implicit none
!
      INCLUDE 'gsnoop_com.i'
!
      integer*2     i, j, k,  ipos
      integer*4     ios, ios2, ios3, ios4
      integer*2     rah, ram, decd, decm, decsign
      integer*4     ix, iy
      real*8        raadj,decadj
!
      real*8        ratest, gratest
      real*8        ras, decs, raunc, decunc
      real*8        radec
!
      character*8   sname
      character*80  qstr
      integer*2     iclen,trimlen
      integer*2     icread
!
      integer*2 icol_adj
      real*8 fjldy
!
      character*150 cbuf_upper
!
!
!     Now we read through the list of global source positions.
!     Reading the global source coords into temporary variables then
!     transfering them into the arrays in common while doing a bubble
!     sort on RA.
!     First cbuf was read in gsn-main.
!
      ix = 1
      iy = 6
      call setcr_mn (ix, iy)
      write (qstr ,'("Getting global source positions.         ")')
      call as2nl(qstr)
      i = 0
      ra_hr(1) = 24
      ra_min(1) = 59
      ra_sec(1) = 59.99999999d0
!
      icol_adj = 0
      if (spool_solve_revision_date_jd .ge. changeover_jd(1)) &
     &  icol_adj = 1
!
      Do while ((cbuf(16:23) .ne. "RT. ASC.") &
     &     .and.(cbuf(17+icol_adj:24+icol_adj) .ne. "RT. ASC."))
        Read(40,'(A)',iostat=ios,end=911) cbuf
        if (ios .ne. 0) then
          write (qstr ,"('Error ',i4, 'reading spoolfile')") ios
          call asnl(qstr)
          write (qstr , "('<RETURN> to continue.')")
          call asnl(qstr)
          call getstr_f(qstr)
        end if
      end do
!
!
      Do while ((cbuf(16:23) .eq. "RT. ASC.") &
     &    .or.  (cbuf(17+icol_adj:24+icol_adj) .eq. "RT. ASC."))
        cbuf_upper = cbuf
        call casefold(cbuf_upper)
        if (index(cbuf_upper,"VELOCITY").ne.0) then
!         Found a source proper motion estimate line (right ascension).
!         Skip this, as well as its declination and correlation lines.
          Read(40,'(A)',iostat=ios) cbuf
          if (ios .ne. 0) then
            write (qstr ,"('Error ',i4, 'reading spoolfile')") ios
            call asnl(qstr)
            write (qstr , "('<RETURN> to continue.')")
            call asnl(qstr)
            call getstr_f(qstr)
          end if
          Read(40,'(A)',iostat=ios) cbuf
          if (ios .ne. 0) then
            write (qstr ,"('Error ',i4, 'reading spoolfile')") ios
            call asnl(qstr)
            write (qstr , "('<RETURN> to continue.')")
            call asnl(qstr)
            call getstr_f(qstr)
          end if
        else
          i = i + 1
          if (cbuf(16:23) .eq. "RT. ASC.") ipos = 1
          if (cbuf(17+icol_adj:24+icol_adj) .eq. "RT. ASC.") ipos = 2
          if (ipos .eq. 1) read(cbuf(7:14),'(a)') sname
          if (ipos .eq. 2) read(cbuf(8+icol_adj:15+icol_adj),'(a)') sname
          if (kdebug) then
            write(qstr,'("Found ",a8, i4)') sname, i
            call asnl(qstr)
          end if
          read(cbuf(33+icol_adj:),*) rah, ram,ras
          read(cbuf(54+icol_adj:),*,iostat=ios2) raadj
          if (ios2.ne. 0) then
            ix = 0
            iy = 20
            call setcr_mn(ix, iy)
            qstr = 'error reading rt asc adj for '//sname
            call beep_mn()
            call blink_on_mn()
             call asnl(qstr)
            call blink_off_mn()
            raadj = 999999.9999d0
          end if
          read(cbuf(76+icol_adj:),*,iostat=ios2) raunc
          if (ios2.ne. 0) then
            ix = 0
            iy = 20
            call setcr_mn(ix, iy)
            qstr = 'error reading rt asc unc for '//sname
            call beep_mn()
            call blink_on_mn()
             call asnl(qstr)
            call blink_off_mn()
            raunc = 149999.99985000d0
          end if
          Read(40,'(A)',iostat=ios) cbuf
          if (ios .ne. 0) then
            write (qstr ,"('Error ',i4, 'reading spoolfile')") ios
            call asnl(qstr)
            write (qstr , "('<RETURN> to continue.')")
            call asnl(qstr)
            call getstr_f(qstr)
          end if
          if (ipos .eq.1) then
            read(cbuf(33:),'(2i3)') decd, decm
            read(cbuf(39:),*)  decs
            if (index(cbuf(33:46),'-').ne.0) then
              decsign = -1  !negative declination
            else
              decsign = 1
            endif
            read(cbuf(62:),*) decadj
          endif
          if (ipos .eq. 2) then
            read(cbuf(34+icol_adj:),'(2i3)') decd, decm
            read(cbuf(41+icol_adj:),*,iostat=ios4) decs
            if (ios4.ne. 0) then
              ix = 0
              iy = 21
              call setcr_mn(ix, iy)
              qstr = 'error reading dec seconds position for '//sname
              call beep_mn()
              call blink_on_mn()
              call asnl(qstr)
              call blink_off_mn()
              decs = 99.9999999D0
            end if
            if (index(cbuf(34+icol_adj:50+icol_adj),'-').ne.0) then
              decsign = -1  !negative declination
            else
              decsign = 1
            endif
            read(cbuf(58+icol_adj:),*,iostat=ios4) decadj
            if (ios4.ne. 0) then
              ix = 0
              iy = 21
              call setcr_mn(ix, iy)
              qstr = 'error reading dec adj for '//sname
              call beep_mn()
              call blink_on_mn()
              call asnl(qstr)
              call blink_off_mn()
              decadj = 999999.9999D0
            end if
          endif
!         Carry positives in the declination value variables, since decsign
!         will take care of the sign.
          decd = habs(decd)
          decm = habs(decm)
          decs = dabs(decs)
          read(cbuf(81+icol_adj:),*,iostat=ios3) decunc
          if (ios3.ne. 0) then
            ix = 0
            iy = 20
            call setcr_mn(ix, iy)
            qstr = 'error reading dec unc for '//sname
            call beep_mn()
            call blink_on_mn()
             call asnl(qstr)
            call blink_off_mn()
            decunc = 9999.9999
          end if
          Read(40,'(A)',iostat=ios) cbuf
          if (ios .ne. 0) then
            write (qstr ,"('Error ',i4, 'reading spoolfile')") ios
            call asnl(qstr)
            write (qstr , "('<RETURN> to continue.')")
            call asnl(qstr)
            call getstr_f(qstr)
          end if
          if (cbuf(16:23).eq."CORRELAT".or. &
     &        cbuf(17+icol_adj:24+icol_adj) .eq. "CORRELAT") then
            if (spool_solve_revision_date_jd .ge. changeover_jd(2)) then
              read (cbuf(28+icol_adj:39), *) radec
            else
              read (cbuf(28+icol_adj:), *) radec
            endif
            Read(40,'(A)',iostat=ios) cbuf   !read blank line
            if (ios .ne. 0) then
              write (qstr ,"('Error ',i4, 'reading spoolfile')") ios
              call asnl(qstr)
              write (qstr , "('<RETURN> to continue.')")
              call asnl(qstr)
              call getstr_f(qstr)
            end if
          else
            radec = 0.0
          end if
!
!
!
!         convert adjs and uncs to sec and arc sec
!         raunc is in milliarcseconds at the equatorial plane.
          raadj = raadj / (15000.0d0)
          decadj = decadj/1.0d3
          raunc = raunc / (15000.0d0)
          decunc = decunc/1.0d3
!
!         do bubble sort
          ratest = 2
          gratest = 1
          j = 1
          do while (ratest .gt. gratest)
            gratest =  8.64d4*ra_hr(j) + 60*ra_min(j) + ra_sec(j)
            ratest  =  8.64d4*rah      + 60*ram       + ras
            if (kdebug) then
              write(qstr,'("gratest ",2(i2,x),f7.3,3x,f9.3)') &
     &              ra_hr(j), ra_min(j), ra_sec(j), gratest
              call addstr_f(qstr)
              write(qstr,'("ratest ",2(i2,x),f7.3,3x,f9.3)') &
     &              rah, ram, ras, ratest
              call addstr_f(qstr)
            end if
            if (ratest .lt. gratest) then
!             move everything back 1 until after we hit j
              do k = i, j, -1
                gsrcname(k+1) = gsrcname(k)
                ra_hr(k+1)  = ra_hr(k)
                ra_min(k+1) = ra_min(k)
                ra_sec(k+1) = ra_sec(k)
                ra_unc(k+1) = ra_unc(k)
                ra_dec(k+1) = ra_dec(k)
                ra_adj(k+1) = ra_adj(k)
                isign_dec(k+1) = isign_dec(k)
                dec_deg(k+1) = dec_deg(k)
                dec_min(k+1) = dec_min(k)
                dec_sec(k+1) = dec_sec(k)
                dec_unc(k+1) = dec_unc(k)
                dec_adj(k+1) = dec_adj(k)
              end do
!
!             overwrite jth entry with new jth entry
              gsrcname(j) = sname
              ra_hr(j) = rah
              ra_min(j) = ram
              ra_sec(j) = ras
              ra_unc(j) = raunc
              ra_dec(j) = radec
              ra_adj(j) = raadj
              isign_dec(j) = decsign
              dec_deg(j) = decd
              dec_min(j) = decm
              dec_sec(j) = decs
              dec_unc(j) = decunc
              dec_adj(j) = decadj
            else
              j = j + 1
            end if
          end do
        endif
!
!         read next source
!       Note that newer spool files may have a blank line
!       between the information lines for each source.
!
        iclen = 0
        do while (iclen.eq.0)
          Read(40,'(A)',iostat=ios) cbuf
          if (ios .ne. 0) then
            write (qstr ,"('Error ',i4, 'reading spoolfile')") ios
            call asnl(qstr)
            write (qstr , "('<RETURN> to continue.')")
            call asnl(qstr)
            call getstr_f(qstr)
          end if
          iclen = trimlen(cbuf)
        enddo
      Enddo
!
      nsrc = i
      ix = 1
      iy = 7
      call setcr_mn (ix, iy)
      write (qstr ,'("# sources = ",i4)') nsrc
      call asnl(qstr)
!
      return
 911  write (qstr , "('EOF encountered befor global sources found.')")
      call asnl(qstr)
      write (qstr , "('<RETURN> to continue.')")
      call asnl(qstr)
      call getstr_f(qstr)
      return
      end
