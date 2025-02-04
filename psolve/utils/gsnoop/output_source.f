      SUBROUTINE output_source()
!
!
      implicit none
!
!     modifications:
!
!      1/19/95 kdb One more digit of precision in the RA and Declination value
!                  and uncertainty fields.
!      2/27/95 kdb Handle overflow in the declination uncertainty field in
!                  the IERS source file.
!      9/29/95 kdb For iers source table, optionally use only sessions with
!                  good observations for the output epochs and session count.
!     10/16/95 kdb One more digit of precision in the RA and Declination
!                  seconds and uncertainty fields and in ra/dec correlations
!                  (iers source table only).
!      3/18/96 kdb Flag new error condition in output file: problem with
!                  declination seconds position.
!      2/6/98  kdb Fix error in which the sign of low declination
!                  Southern sources is not printed.  (Have redone the scheme
!                  for carrying the sign of the declination so that the
!                  three values, degrees, minutes and seconds are positive,
!                  and the sign is carried separately.)
!      9/14/01 kdb Optionally suppress source lines with warnings (e.g.,
!                  "HIGH UNCERTAINTY") in both the iers_src_<> and
!                  srcmod_<> files.
!      3/24/03 kdb New output file, source_basic_stats_<>.
!
      INCLUDE 'gsnoop_com.i'
      integer*2 i, j
      integer*2 ideg, imin
      integer*2 jdeg, jmin
      integer*4 trimlen, len
!
!
      real*8  xsec, ddec, dra, rdra
      real*8  epoch_coord
      parameter (epoch_coord = 51544.5)  !AD 2000
!
      character*1   qsign                  !algebraic sign
      character*72  qwarn
      character*8   iauname
      character*80  qstr
      parameter (iauname = '0       ')
      real*8 decunc_print
      real*8 decs_print
      integer*2 src_sum_print_sess
      character*3 dec_deg_field
      character*1 suppress_warning_lines
      logical*2 suppress_this_line
!
!      Write output.
!
      write (qstr , "('Writing the source tables.')")
      call asnl(qstr)
      if (do_3_sig) write (qstr , "('With 3-sigma errors')")
      call asnl(qstr)
      if (source_flyby) then
         Write(51,'("$$",/"$$",/,"$$ VLBI source positions ",/"$$")')
      end if
!
      if (do_iers_source) then
        Write(47,'(/" VLBI source positions for epoch ",f8.1," MJD"/)') &
     &                 epoch_coord
        if (do_3_sig) Write(47,"('(Errors in this table are 3-sigma)')")
        Write(47,'("source name      |Right  Ascension |", &
     &   "  Declination    ", &
     &   "|  Uncertainties     | cor  | mean  | first | last  |", &
     &   "  observations ")')
!
        Write(47,'("IAU name|IVS name|Hr|Mn|  Seconds  |Deg|Mn|", &
     &   " Seconds  |  Unc RA  |Unc Dec  |Ra-Dc |  MJD  |  MJD  |", &
     &   "  MJD  |", &
     &   "sess |delay |rates")')
!
        Write(47,'("        |        |  |  |           |   |  |", &
     &   "          |          |         |      |       |       |" &
     &   "      |     |      |")')
!
!       Basic source statistics file.
!
        Write(115,'(/" VLBI source positions for epoch ",f8.1," MJD"/)') &
     &                 epoch_coord
        if (do_3_sig) Write(115,"('(Errors in this table are 3-sigma)')")
        Write(115,'("source name      |Right  Ascension |", &
     &   "  Declination    ", &
     &   "|      Adjustments     |  Uncertainties     |  cor  |")')
!
        Write(115,'("IAU name|IVS name|Hr|Mn|  Seconds  |Deg|Mn|", &
     &   " Seconds  |  Adj RA  |  Adj Dec  |  Unc RA  |Unc Dec  | Ra-Dc |")')
        Write(115,'("                 |                 |", &
     &   "                 ", &
     &   "|                      |                    |       |")')
!
!
      end if
!
      write(qstr,"('Suppress lines with warnings')")
      call asnl(qstr)
      write(qstr,"('  (e.g., NO DATA or HIGH UNCERTAINTY) (y/n)?  ')")
      call asnl(qstr)
      call getstr_f(qstr)
      read (qstr,"(a1)") suppress_warning_lines
      call casefold(suppress_warning_lines)
!
      do i = 1, nsrc
!
!       Loop through the epoch information arrays until we match
!       srcname with gsrcname.
!
        j = 1
        do while ((gsrcname(i) .ne. srcname(j)).and.(j .le. totsrc))
          j = j + 1
        end do
!
!       Check if declination north or south and set qsign accordingly
!
        if (isign_dec(i).lt. 0) then
          qsign = "-"
        else
          qsign = " "
        end if
!
!
        if ((src_sum_dlay(j) .eq. 0).and.(src_sum_rate(j) .eq. 0)) then
          write (qwarn(1:),"('NO DATA')")
        else if (dec_sec(i) .gt. 99.9999D0) then
          write(qwarn(1:),"('POSITION OVERFLOW')")
        else if (ra_unc(i) .gt. 9d0)  then
          write (qwarn(1:),"('UNCERTAINTY OVERFLOW')")
        else if (dec_unc(i) .gt. 9d0)  then
          write (qwarn(1:),"('UNCERTAINTY OVERFLOW')")
        else if ((ra_unc(i) .ge. 1.0d-3).or.(dec_unc(i) .ge. 1.0d-2)) &
     &  then
          write (qwarn(1:),"('HIGH UNCERTAINTY')")
        else if ((ra_dec(i) .ge. 1.0d0).or.(dec_unc(i) .le.-1.0d02)) &
     &  then
          write (qwarn(1:),"('CORRELATION EQUALS OR EXCEEDS UNITY')")
        else
          write (qwarn(1:), "(72x)")
        end if
        len = trimlen(qwarn) + 1
        if (suppress_warning_lines.eq.'Y'.and.qwarn(1:1).ne.' ') then
          suppress_this_line = .true.
        else
          suppress_this_line = .false.
        endif
!
        if ((src_sum_sess(j) .ne. 0).or.(nsrc .gt. totsrc)) then  !YES (j)
          jdeg = dec_deg(i)
          jmin = dec_min(i)
          xsec = dec_sec(i)
!
          if (do_iers_source) then
            if (use_bad_src_sess.eq.'Y') then
              src_sum_print_sess =  src_sum_sess(j)
            else
              src_sum_print_sess =  src_sum_good_sess(j)
            endif
            if (dec_unc(i)*scale .lt. 10.0D0) then
              decunc_print = dec_unc(i) * scale
            else
              decunc_print = 9.9999999
            end if
            if (.not.suppress_this_line) then
              if (ra_dec(i) .eq. -1.00d0) then
                write(47,"(2(a8,1x),2(i2.2,1x),f11.8,1x,a1,2(i2.2,x), &
     &          f10.7,1x,f10.8,1x,f9.7,1x,f6.3,1x,3(f7.1,1x), &
     &          1x,i4,1x,i6,1x,i6,x,a)") &
     &          iauname, &
     &          gsrcname(i), &
     &          ra_hr(i), &
     &          ra_min(i), &
     &          ra_sec(i), &
     &          qsign, &
     &          jdeg, &
     &          jmin, &
     &          xsec, &
     &          ra_unc(i)*scale, &
     &          decunc_print, &
     &          ra_dec(i), &
     &          src_mean_epoch(j), &   !YES I mean (j)
     &          src_mjdmin(j), &
     &          src_mjdmax(j), &
     &          src_sum_print_sess, &
     &          src_sum_dlay(j), &
     &          src_sum_rate(j), &
     &          qwarn(1:len)
!
              else
                write(47,"(2(a8,1x),2(i2.2,1x),f11.8,1x,a1,2(i2.2,x), &
     &          f10.7,1x,f10.8,1x,f9.7,1x,f6.4,1x,3(f7.1,1x), &
     &          1x,i4,1x,i6,1x,i6,x,a)") &
     &          iauname, &
     &          gsrcname(i), &
     &          ra_hr(i), &
     &          ra_min(i), &
     &          ra_sec(i), &
     &          qsign, &
     &          jdeg, &
     &          jmin, &
     &          xsec, &
     &          ra_unc(i)*scale, &
     &          decunc_print, &
     &          ra_dec(i), &
     &          src_mean_epoch(j), &   !YES I mean (j)
     &          src_mjdmin(j), &
     &          src_mjdmax(j), &
     &          src_sum_print_sess, &
     &          src_sum_dlay(j), &
     &          src_sum_rate(j), &
     &          qwarn(1:len)
              end if
              write(115,"(2(a8,1x),2(i2.2,1x),f11.8,1x,a1,2(i2.2,x), &
     &          f10.7,1x, &
     &          f10.8,1x,f11.7,1x, &
     &          f10.8,1x,f9.7,1x,f7.4,x,a)") &
     &          iauname, &
     &          gsrcname(i), &
     &          ra_hr(i), &
     &          ra_min(i), &
     &          ra_sec(i), &
     &          qsign, &
     &          jdeg, &
     &          jmin, &
     &          xsec, &
     &          ra_adj(i), &
     &          dec_adj(i), &
     &          ra_unc(i)*scale, &
     &          decunc_print, &
     &          ra_dec(i), &
     &          qwarn(1:len)
            end if
          end if
        end if
!
        if (src_plt) then
          if ((ra_unc(i) .gt. 0.0d0).or.(dec_unc(i) .gt. 0.0d0)) then
            ddec = dec_deg(i) + (dec_min(i) + dec_sec(i)/60.0d0)/60.0d0
            if (qsign.eq.'-') ddec = ddec * -1.0D0
            dra  = ra_hr(i)   + (ra_min(i)  + ra_sec(i)/60.0d0)/60.0d0
            rdra = 360.0 - dra
            write(81,"(3(f15.10,3x),8a)") &
     &        dra, &
     &        rdra, &
     &        ddec, &
     &        gsrcname(i)
!
          end if
        end if
!
!
        if (source_flyby) then !  do only those with good data vvvvv
!         Truncate to fit in the field
          decs_print = dec_sec(i)
          if (dec_sec(i).gt.99.99999D0) decs_print = 99.99999D0
          imin = dec_min(i)
          ideg = dec_deg(i)
          dec_deg_field = '   '
          write(dec_deg_field(2:3),"(i2)") ideg
          if (qsign.eq.'-') then
            if (ideg.ge.10) then
              dec_deg_field(1:1) = '-'
            else
              dec_deg_field(2:2) = '-'
            endif
          endif
          if (.not.suppress_this_line) then
            if ((ra_unc(i) .gt. 0.0d0).or.(dec_unc(i) .gt. 0.0d0)) then
!
              write(51,"(4x,a8,2x,2(i2,x),f10.7,4x,a3,i3,f9.5,10x,a)") &
     &        gsrcname(i), &
     &        ra_hr(i), &
     &        ra_min(i), &
     &        ra_sec(i), &
     &        dec_deg_field, &
     &        imin, &
     &        decs_print, &
     &        qwarn(1:len)
            else   !  put in mod file as a comment only
!
              write(51, &
     &        "('$$',2x,a8,2x,2(i2,x),f10.7,4x,a3,i3,f9.5,10x,a)") &
     &        gsrcname(i), &
     &        ra_hr(i), &
     &        ra_min(i), &
     &        ra_sec(i), &
     &        dec_deg_field, &
     &        imin, &
     &        decs_print, &
     &        qwarn(1:len)
            end if
          end if
        end if
!
      end do
      return
      end
