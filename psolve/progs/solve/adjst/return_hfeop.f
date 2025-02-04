      SUBROUTINE RETURN_HFEOP ( TIME_JD, DUT, DXWOB, DYWOB, &
     &                          DUTD, DXWOBD, DYWOBD )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     This routine is an integral part of adjst and it is used get the hfeop
!     values given a jd.  This is a computation used many times in a2jst
!     and this hides the details.
!
!     Input:
      real*8    time_jd   ! The jd to look up the values.
      integer*2 khfeop    ! =0 --> no hheop, =1 --> hfeop on.
!
!     output:
      real*8   dut        ! UT1 correction         (s)
      real*8   dxwob      ! x-pole correction      (radians)
      real*8   dywob      ! y-pole correction      (radians)
      real*8   dutd       ! UT1 rate correction    (s/day)
      real*8   dxwobd     ! x-pole rate correction (radians/day)
      real*8   dywobd     ! y-pole rate correction (radians/day)
!
      real*8   tc2000,cnvrt
!
!     mas/Radians (for wobble)
      data cnvrt /206264806.d0/
!
!     :97.03.06:jwr: Created.
!     :97.11.28:pet: Changed argument for get_hf_eop from JUL_CENT to JUL_DATE
!                    inaccording woth caneges in the get_hf_eop routine
!
! --- Get the high frequency eop corrections. They may be zero.
!
      CALL GET_HF_EOP ( TIME_JD, DUT, DXWOB, DYWOB, DUTD, DXWOBD, DYWOBD, &
     &                  INT2(1) )
!
! --- Convert ut from milliseconds to seconds
!
      DUT = DUT/1.D3
!
! --- Convert x&y pole from mas to radians
!
      DXWOB =  DXWOB/CNVRT
      DYWOB =  DYWOB/CNVRT
!
! --- Convert x&y pole rates from mas/s to radians/day
!
      DXWOBD= (DXWOBD/CNVRT)*86400.d0
      DYWOBD= (DYWOBD/CNVRT)*86400.d0
!
! --- Convert ut1 rate from s/s to s/day
!     DUTD = DUTD*86400.D0
!
      RETURN
      END  !#!  RETURN_HFEOP  #!#
