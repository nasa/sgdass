      SUBROUTINE PROPAGATE ( x0,covar0,num_parm,delta_t,x_pred,covar_pred)
      implicit none
! propagate x0,covar0 forward (or backward) by t.
      INTEGER*2 num_parm
      REAL*8      x0(num_parm)    !Original Filter estimate
      REAL*8      Covar0(num_parm*(num_parm+1)/2) !And covariance
!
      REAL*8     delta_t          !Time interval to propagate
      REAL*8     x_pred(num_parm) !Estimate at new time
      REAL*8     covar_pred(num_parm*(num_parm+1)/2)  !and covariance
!
! local variables.
      REAL*8      phi(num_parm,num_parm)   !Propagator.
!
      INTEGER*4 indx4
      INTEGER*2 i,j,k,l,ij
!
      call make_phi(phi,delta_T)
! propagate x0:  x_pred= Phi X0
      x_pred=0.
      do i=1,num_parm
         do j=1,num_parm
            x_pred(i)=x_pred(i)+phi(i,j)*x0(j)
         end do
      end do
!
! propagate covar0:  covar_pred=covar_noise+Phi covar0 phi^T
      call make_noise_covar(covar_pred,delta_T)
      do i=1,num_parm
      do j=1,i
         ij=indx4(i,j)
         do k=1,num_parm
         do l=1,num_parm
            covar_pred(ij)=covar_pred(ij)+ &
     &                       phi(i,k)*covar0(indx4(k,l))*phi(j,l)
         end do
         end do
      end do
      END do
!
      RETURN
      END  !#!  PROPAGATE  #!#
