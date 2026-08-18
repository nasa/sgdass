      SUBROUTINE make_noise_covar(noise_covar,t)
! here we make the noise covarinace matrix.
      implicit none
      INCLUDE 'eopkal_ut.i'
      INCLUDE 'eopkal_pm.i'
!
      REAL*8     noise_covar((num_ut+num_pm)*(num_ut+num_pm+1)/2)
!
!
! transition matrix decomposes into two pieces: One for PM,one for UT
      REAL*8     t
      REAL*8     noise_covar_ut(num_ut*(num_ut+1)/2)
!
      INTEGER*4 indx4
      INTEGER*2 ij_in,ij_out           !pointers into covar_matrices
      INTEGER*2 i                      !counter
      INTEGER*2 i1/1/
!
      noise_covar=0.
!
      call make_noise_covar_PM(noise_covar,t)
!
      call make_noise_covar_ut(noise_covar_Ut,t)
! put UT1 noise covar in appropriate place.
      do i=1,num_ut
        ij_out=indx4(num_pm+i1,num_pm+i)
        ij_in=indx4(i1,i)
        noise_covar(ij_out:ij_out+i-1)=noise_covar_ut(ij_in:ij_in+i-1)
      END do
      IF(t .LT. 0) then
        noise_covar=-noise_covar
      endif
      return
      end
