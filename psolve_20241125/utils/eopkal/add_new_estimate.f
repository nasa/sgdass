      SUBROUTINE ADD_NEW_ESTIMATE ( z_meas,cov_meas, &
     &     num_in,x_old,cov_old,num_parm,proj,x_new,cov_new)
      implicit none
! Given
!   1. A measurement of some parameters, and their covariance
!   2. Previous estimate of parameters, and their covariance
! Find
!   Best guess of parameters.  Note: not all parameters are measured.
! Schematically, we have:
!   COV_new=[cov_old^-1 + cov_meas^-1]^-1
!   X_new = [cov_old^-1 + cov_meas^-1]^-1*[cov_old^-1*x_old+cov_meas^-1*z_meas]
!
! INPUT:
      INTEGER*2 num_in                           !Number of things measured
      REAL*8     z_meas(num_in)            !Measurements
      REAL*8     cov_meas(num_in*(num_in+1)/2)   !Covariances
      INTEGER*2 num_parm                         !Number of parameters total
      REAL*8     x_old(num_parm)           !Old Filter estimates
      REAL*8     cov_old(num_parm*(num_parm+1)/2)    !& Covariances
      REAL*8     proj(num_parm,num_in)     !Z_meas --->x_filt
!
      REAL*8     x_new(num_parm)           !Filter estimates
      REAL*8     cov_new(num_parm*(num_parm+1)/2)     !& Covariances
!
! Local variables.
! temporary arrays to hold info
      REAL*8     z_meas_tmp(num_in)        !Measurements
      REAL*8     cov_meas_tmp(num_in*(num_in+1)/2)   !Covariances
!
      INTEGER*4 indx4
      INTEGER*2 ix,lx, jz,kz,il_x         !Matrix indices.
!
      z_meas_tmp=z_meas
      cov_meas_tmp=cov_meas
      call invert_norm_tri(cov_meas_tmp,z_meas_tmp,num_in)
!
      x_new=x_old
      cov_new=cov_old
      call invert_norm_tri(cov_new,x_new,num_parm)
!
!     x_new=x_new+proj*z_meas_tmp
      do ix=1,num_parm
        do jz=1,num_in
           x_new(ix)=x_new(ix)+proj(ix,jz)*z_meas_tmp(jz)
        END do
      END do
!
!     Cov_new=cov_new + proj cov_meas_tmp proj^T
      do ix=1,num_parm
      do lx=1,ix
         il_x=indx4(ix,lx)
         do jz=1,num_in
         do kz=1,num_in
            cov_new(il_x)=cov_new(il_x)+ &
     &            proj(ix,jz)*cov_meas_tmp(indx4(jz,kz))*proj(lx,kz)
         end do
         end do
      end do
      end do
!
      call invert_norm_tri(cov_new,x_new,num_parm)
!
      RETURN
      END  !#!  ADD_NEW_ESTIMATE  #!#
