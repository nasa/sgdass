      SUBROUTINE qant_type(qname, num, itype)
!
!
      IMPLICIT NONE
      INCLUDE 'gsnoop_com.i'
      INCLUDE 'solve.i'
!
      integer*2     num              !number of antennas in qname
      integer*2     itype(max_sta_gsnoop), n
      integer*4     ios
      CHARACTER*1   qcdp, qiers   !CDP, IERS monument codes
!      CHARACTER*1   qmon(max_sta_gsnoop) !"A" or "G" depending on antenna type
      character*8   qname(max_sta_gsnoop)
      character*40  qbuffer
      character*80  qstr
!
!     modifications
!
!     98/02/27 kdb Move antenna.dat to solve_files directory for use by Solve.
!     00/10/25 kdb Change max_sta to max_sta_gsnoop.
!
!
!       Check antenna.dat to find if fixed or mobile
!
!       itype:    0 = Error
!                 1 = Fixed antenna monumented at intersection of axes
!                 2 = Mobile or transportable antenna, ground monument
!                 3 = Fixed antenna, ground monument
!                 4 = Mobile or transportable antenna
!                     monumented at intersection of axes
!
      write (qbuffer, '(40x)')
!
      open (49,file=SOLVE_SAVE_DIR//'antenna.dat')
!
!     Main loop.
!
      do n = 1, num
!
          rewind (49)
          do while (qname(n) .ne. qbuffer(1:8))
            read (49, "(a)", end=910, iostat=ios) qbuffer
          end do
!
          qcdp  = qbuffer(15:15)
          qiers = qbuffer(22:22)
!
          if ((qcdp  .eq. "m").or.(qcdp .eq. "M")) then
            if ((qiers .eq. "s").or.(qiers .eq. "S")) then
              itype(n) = 4
            else if ((qiers .eq. "M").or.(qiers .eq. "m")) then
              itype(n) = 2
            end if
!
          else if ((qcdp  .eq. "f").or.(qcdp .eq. "F")) then
            if ((qiers .eq. "s").or.(qiers .eq. "S")) then
              itype(n) = 1
            else if ((qiers .eq. "M").or.(qiers .eq. "m")) then
              itype(n) = 3
            end if
          else
            itype(n) = 0
          end if
!
 910      if (ios .ne. 0) then
            write (qstr , "('station ',a8,' not found in list.')") &
     &             qname(n)
            call as2nl(qstr)
          end if
      end do ! n =
!
      close (49)
!
      return
      end
