        SUBROUTINE get_hf_ut1(JUL_CEN,dut)
       implicit none
! History:
!  WRitten Sept 30, 1992
!    JMGipson
! on entry
!   Jul_cen -- time in julian centuries from 2000.
! on exit
!   dut     -- change in ut1 in seconds.
! base on fitting several years of VLBI data.
!
        real*8 jul_cen,dut,time_hrs,dot8
        real*8 f(16)
!
!
!  cosine and sine amplitude terms.    In ms.
        real*8 tide_amp(16)
        data tide_amp/ &
     &     -0.001843, 0.004364,-0.000728, 0.002776,-0.011250, 0.009758, &
     &      0.000360,-0.000608, 0.000253,-0.000665,-0.000167,-0.005487, &
     &      0.011255,-0.003652,-0.001480,-0.001171 /
!
! period of tidal term in hours.
        real*8 period(8)
        data period/23.94,24.07,25.82,26.88, &
     &            11.97,12.00,12.42,12.66/
!
        time_hrs=36525.d0*24.d0*jul_cen
        call get_basis(f,time_hrs,period,16)
! get correction in ms, and convert to seconds.
        dut = dot8(f,tide_amp,16)/1.d3
        return
        end
!********************************************************
        SUBROUTINE get_basis(f,x,period,iparm)
        real*8 x
        real*8 f(*),period(*)
        real*8 twopi/6.28318530717959D0/
! number of periodic terms.
        iterm=(iparm)/2
!
        do it=1,iterm
            f(2*it-1)  =dcos(twopi*x/period(it))
            f(2*it)=dsin(twopi*x/period(it))
        end do
        return
        end
!********************************************************
         FUNCTION dot8(a,b,ilen)
         real*8 dot8
!
         real*8 a(*),b(*)
         dot8=0.
         do i=1,ilen
           dot8=dot8+a(i)*b(i)
         end do
         return
         end
!
