      SUBROUTINE INIT_FILTER ( x_init,cov_init,num_parm)
! initialize filter.
      IMPLICIT NONE
      INCLUDE 'eopkal_ut.i'
      INCLUDE 'eopkal_pm.i'
      INTEGER*2 num_parm
      REAL*8     x_init(num_parm)                    !Parameters
      REAL*8     cov_init(num_parm*(num_parm+1)/2)   !covariances
!
! Local variables
      INTEGER*4 indx4
      INTEGER*2 iparm
!
! initial guess is 0, with huge uncertainties
      cov_init=0.
      do iparm=1,num_parm
        x_init(iparm)=0.
        cov_init(indx4(iparm,iparm))=12.d8
      end do
! initilize dut1/dt
      x_init(num_pm+2)=-2000.
!
! initialize filter parameters
      call init_ut()
      call init_pm()
!
      RETURN
      END  !#!  INIT_FILTER  #!#
