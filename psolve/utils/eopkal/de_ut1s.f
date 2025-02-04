      SUBROUTINE DE_UT1S ( fjd_meas,z_meas,num_eop,kuse_rate,num_meas)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
      INTEGER*2 num_eop                     !number of EOP measured
      INTEGER*4 num_meas                    !number of measurements
      REAL*8     fjd_meas(*)          !Epoch
      REAL*8     z_meas(num_eop,*)    !EOP Values
      LOGICAL*2 kuse_rate(3)                  !X,Y,UT1 rate flags
!
! local variables
      REAL*8     fa(5),fad(5)         !Fundamental nutation arguments
      REAL*8     dut,dut_dot          !change in dut1,dut_dot
      REAL*8     cent_julian          !returned from nutfa. Centuries from J2000.
      REAL*8     DOMEGA
      INTEGER*4 imeas                       !counter
!
! UT1 is always 3rd entry.
! UT1_dot is always last entry, if we have it on.
!
      do imeas=1,num_meas
        call nutfa(fjd_meas(imeas),0.d0,cent_julian,fa,fad )
        call ut1s_83 ( fa, fad, dut, dut_dot, domega )
        z_meas(3,imeas)=z_meas(3,imeas)-dut*1.d6
        IF(kuse_rate(3)) &
     &      z_meas(num_eop,imeas)=z_meas(num_eop,imeas)-dut_dot*1.d6
      END do
      RETURN
      END  !#!  DE_UT1S  #!#
