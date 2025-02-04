      SUBROUTINE INTERPOLATE_CONCRETE ( date,fjd_con,z_con,cov_con, &
     & num_con, eop_vec,eop_cov)
! return interpolated eop_values.
! note: concrete series has daily values!
!
      implicit none
      REAL*8      date
      REAL*8      fjd_con(*)
      REAL*8      z_con(3,*)
      REAL*8      cov_con(6,*)
      integer*4 num_con,iptr
      REAL*8      eop_vec(4),eop_cov(10)
      REAL*8      temp,frac
!
      temp = date-fjd_con(1)+1
      iptr = int(temp)
      if(iptr .le. 0) then
        write(*,*) "Interpolate_concrete:  Value before start of data!"
        stop
      else if(iptr .ge. num_con) then
        write(*,*) "Interpolate_concrete:  Value after end of data!"
        stop
      endif
! now do simple minded interpolation.
      frac = temp-iptr
      eop_vec(1:3)=(1.d0-frac)*z_con(1:3,iptr)+frac*z_con(1:3,iptr+1)
      eop_cov(1:6)=(1.d0-frac)*cov_con(1:6,iptr) &
     &                         +frac*cov_con(1:6,iptr+1)
!
      RETURN
      END  !#!  INTERPOLATE_CONCRETE  #!#
