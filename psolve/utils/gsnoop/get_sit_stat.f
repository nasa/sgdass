!
      SUBROUTINE get_sit_stat (ista, oldtotsta, iexp_sta_used, station)
!
!     Subroutine to get mean epoch & span of obsevations for stations
!     and sources in the spoolfile.
!     DS Caprette 3/9/91
!     Stations list should be sorted befor coming here, otherwise you
!     might not be very happy with the results!
!     Added proper handling of sites with episodic motion
!     DS Caprette 92/03/24
!
      IMPLICIT NONE
!
      INCLUDE 'gsnoop_com.i'
!
      INTEGER*2     IOS,I,J,K,m
!
      integer*2     ista
      integer*2     oldtotsta
      integer*2     no_data
      integer*2     max_sta_exp
      parameter     (max_sta_exp = 80) !twice max # baselines possible
!                                         in one experiment
!
      real*8        xjunk
      Integer*4     iexp_sta_used(max_sta_exp)
!
!
      character*8   station(max_sta_exp)
!
!
!     Initialize sums etc
!
!
      oldtotsta = 0
      no_data = 0
!
!
        do while (CBUF(1:20) .ne. ' Baseline Statistics')
          read(40,'(a)', end=999) cbuf
        end do
!
!
        do i = 1,4     ! read up to and including first baseline
          READ(40,'(A)',END=999) CBUF
        end do
!
!         Fill the array of station names for this experiment.
!         Pick up the # observations used for each.  Remember
!         two stations per baseline so exp_stat_obs(1) corresponds
!         to both station(1) and station(2) etc
!         Each station gets counted one for each baseline it is in.
!
        ista = 0
        do while (cbuf(1:9) .ne. "        ")
          ista = ista + 2
          read(cbuf(2:9),'(a)')   station(ista-1)    !#'s 1,3,5,7,9
          read(cbuf(11:18),'(a)') station(ista)      !#'s 2,4,6,8,10
!
          if ( cbuf(21:27) .ne. 'No Data' ) then
               read(cbuf(19:22),*)     xjunk
               iexp_sta_used(ista)   = dint(xjunk)
               iexp_sta_used(ista-1) = iexp_sta_used(ista)
             else
               iexp_sta_used(ista)   = 0
               iexp_sta_used(ista-1) = 0
          end if
!
          if ( iexp_sta_used(ista) .eq. 0) then !No data used
               ista = ista - 2   !So ignore this baseline
               no_data = no_data + 1
          end if
!
          READ(40,'(A)',END=999)   CBUF
        end do
!
!
 999  return
      end
