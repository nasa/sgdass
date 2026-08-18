!
        SUBROUTINE comp_sdc_arg( sd_mult, fund_arg, arg, num_arg )
      implicit none
!
!     This routine computes the argument of the tide given the
!     the fundamental argument values and the multipliers to
!     generate the tide
!
!* PHYSICAL CONSTANTS NEEDED FOR SD_COMP
!
!*   pi          - Define here to full precision
!
        real*8 pi
!
        parameter ( pi            = 3.1415926535897932D0 )
!
!*-------------------------------------------------------------------
!
!* PASSED VARIABLES
!
!* INPUT
!*   sd_mult(6)  - multipliers for Browns arguments and gst+pi
!*   num_arg     - Number of arguments to be summed. (Allows the
!                 gst+pi argument to be skipped.)
!
        integer*2 sd_mult(6), num_arg
!
!*   fund_arg(6) - Values for the fundamental artguments
!
!* OUPUT
!*   arg         - Argumnent at this time (rad)
!
        real*8 fund_arg(6), arg
!
! LOCAL VRAIABLES
!
!   i           - Loop counter
!
        integer*2 i
!
!***C  Initialize argument and combine the fundamental angels with the
!     values passed
!
        arg = 0.d0
        do i = 1, num_arg
            arg = arg + sd_mult(i)*fund_arg(i)
        end do
!
        arg = mod(arg, 2.d0*pi)
!
!***** Thats all
        return
        end
